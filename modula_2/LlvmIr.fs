module modula_2.LlvmIr

open System.Collections.Generic
open System.IO
open System
open System.Diagnostics

open LLVMSharp.Interop
open LLVMSharp

open Ast
open Parser
open Cfg
open Ssa
open Utils
open SimplifyAst
open modula_2.Ast


type Values = Dictionary<int, LLVMValueRef>


type Types = Dictionary<string, Tuple<TypeDefinition, int, LLVMTypeRef>>


type ValIdToType = Tuple<IDictionary<int, string>, Types>


let rec cummulative_sums prev ls =
     match ls with
     | h::tail ->
          prev::(cummulative_sums (prev + h) tail)
     | [] ->
          []


let rec type_description (result: Types) type_def =
     let ptr_type = LLVMTypeRef.CreatePointer(LLVMTypeRef.Int8, 0u) in
     match type_def with
     | Void ->
          Void, 0, LLVMTypeRef.Void
     | Int ->
          type_def, 4, LLVMTypeRef.Int32
     | ArrayDef(elems_number, element_type) ->
          let _, type_size, _ = result[element_type] in
          type_def, elems_number * type_size, ptr_type
     | StructDef(Fields(fields)) ->
          let sizes = [for var, t in fields -> let _, size, _ = result[t] in size] in
          let offsets = cummulative_sums 0 sizes in
          let fields = dict [for (var, t), offset in zip fields offsets -> var, (t, offset)] in
          StructDef(FieldsOffsets(fields)), sum sizes, ptr_type


let get_type_descriptions ast =
     let mutable result = Types() in
     for name, t in ["", Void; "integer", Int] do
          result[name] <- type_description result t
     let type_defs =
          match ast with
          | Module(block = (declarations, _)) ->
               [for declaration in declarations do
                                   match declaration with
                                   | TypeDef(type_name, type_def) ->
                                        result[type_name] <- type_description result type_def
                                   | _ -> ()] in
     result


type LlvmFns = Dictionary<string,(LLVMTypeRef * LLVMValueRef)>


let type_from_phi_or_local (val_id_to_type: ValIdToType) phi_or_local =
     let id = match phi_or_local with Local(id = id) -> id | Phi(operands = (id, _)::_) -> id in
     let id_to_type, type_to_def = val_id_to_type in
     type_to_def[id_to_type[id]]


let accessors_context (builder: LLVMBuilderRef) exp_to_llvm' val_id_to_type phi_or_local field_or_index =
     let type_def, agregate_size, _ = type_from_phi_or_local val_id_to_type phi_or_local in
     let _, type_to_def = val_id_to_type in
     let src = exp_to_llvm' phi_or_local in
     let element_type, offset, element_size =
          match type_def with
          | StructDef(FieldsOffsets(offsets)) ->
               let field_type, field_offset = match field_or_index with Field(field) -> offsets[field] in
               let _, field_size, _ = type_to_def[field_type] in
               field_type,
               LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 field_offset, true),
               LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 field_size, true)
          | ArrayDef(element_type = element_type) ->
               let index = match field_or_index with Index(index) -> exp_to_llvm' index in
               let _, element_size, _ = type_to_def[element_type] in
               let element_size = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 element_size, true) in
               element_type,
               builder.BuildNSWMul(index, element_size),
               element_size in
     let element_type_def, _, llvm_element_type = type_to_def[element_type] in
     src, agregate_size, offset, element_type_def, llvm_element_type, element_size


let rec exp_to_llvm (val_id_to_type: ValIdToType) (fns: LlvmFns) (values: Values) (builder: LLVMBuilderRef) exp =
     let exp_to_llvm' = exp_to_llvm val_id_to_type fns values builder in
     match exp with
     | Binary(left, op, right) ->
          let left, right = exp_to_llvm' left, exp_to_llvm' right in
          match op with
          | "+" -> builder.BuildNSWAdd(left, right)
          | "-" -> builder.BuildNSWSub(left, right)
          | "/" -> builder.BuildSDiv(left, right)
          | "=" -> builder.BuildICmp(LLVMIntPredicate.LLVMIntEQ, left, right)
          | "#" -> builder.BuildICmp(LLVMIntPredicate.LLVMIntNE, left, right)
          | "mod" -> builder.BuildSRem(left, right)
          | ">" -> builder.BuildICmp(LLVMIntPredicate.LLVMIntSGT, left, right)
          | "<" -> builder.BuildICmp(LLVMIntPredicate.LLVMIntSLT, left, right)
     | Unary(op, exp) ->
          builder.BuildXor(LLVMValueRef.CreateConstInt(LLVMTypeRef.Int1, 1UL, false), exp_to_llvm' exp)
     | Local(id = id) | Phi(id = id) ->
          values[id]
     | Number(v) ->
          LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 v, true)
     | Get(phi_or_local, field_or_index) ->
          let src, _, offset, element_type_def, llvm_element_type, element_size = accessors_context builder exp_to_llvm' val_id_to_type phi_or_local field_or_index in
          let fn_type, fn = fns["get"] in
          let ret_val = builder.BuildCall2(fn_type, fn, [| src; offset; element_size |])
          match element_type_def with
          | Int ->
               builder.BuildLoad2(llvm_element_type, ret_val)
          | _ ->
               ret_val
     | Update(phi_or_local, field_or_index, value) ->
          let src, agregate_size, offset, element_type_def, llvm_element_type, element_size = accessors_context builder exp_to_llvm' val_id_to_type phi_or_local field_or_index in
          let value = exp_to_llvm' value in
          let value_ptr =
               match element_type_def with
               | Int ->
                    let value_ptr = builder.BuildMalloc(llvm_element_type) in
                    builder.BuildStore(value, value_ptr)
                    value_ptr
               | _ ->
                    value in
          let fn_type, fn = fns["update"] in
          builder.BuildCall2(fn_type, fn, [|
               src
               LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 agregate_size, true)
               offset
               value_ptr
               element_size
          |])
     | FnCall'(name, args) ->
          let fn_type, fn = fns[name] in
          let args = toArray [for arg in args -> exp_to_llvm' arg] in
          builder.BuildCall2(fn_type, fn, args)
     | ArrayValue(ls) ->
          let size = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 ls.Length, false) in
          let array = builder.BuildArrayMalloc(LLVMTypeRef.Int32, size) in
          let ls = [for v in ls -> exp_to_llvm' v] in
          for idx, elem in enumerate(ls) do
               let idx = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 idx, false) in
               let elem_ptr = builder.BuildInBoundsGEP2(LLVMTypeRef.Int32, array, [| idx |]) in
               builder.BuildStore(elem, elem_ptr)
          array
     | Malloc(type_def) ->
          match type_def with
          | ArrayDef(elems_number, element_type) ->
               let size = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 elems_number, false) in
               builder.BuildArrayMalloc(LLVMTypeRef.Int32, size)
          | StructDef(FieldsOffsets(fields)) ->
               let size = sum [for item in fields -> let _, size = item.Value in size] in
               let size = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int32, uint64 size, false) in
               builder.BuildArrayMalloc(LLVMTypeRef.Int8, size)


let build_phis (builder: LLVMBuilderRef) (values: Values) (val_id_to_type: ValIdToType) phis blockId =
     for phi in get phis blockId [] do
         match phi with
         | Phi(id = id; operands = (id', _)::_) ->
              let id_to_type, type_to_def = val_id_to_type in
              let _, _, val_type = type_to_def[id_to_type[id']] in
              let phi' = builder.BuildPhi(val_type, $"{id}") in
              values[id] <- phi'


let create_print (builder: LLVMBuilderRef) (mdl: LLVMModuleRef) =
     let format_string = builder.BuildGlobalStringPtr("%d\n") in
     let ptr_type = LLVMTypeRef.CreatePointer(LLVMTypeRef.Int8, 0u) in
     let printf_type = LLVMTypeRef.CreateFunction(LLVMTypeRef.Int32, [|ptr_type|], true) in
     let printf_fn = mdl.AddFunction("printf", printf_type)
     let fn_type = LLVMTypeRef.CreateFunction(LLVMTypeRef.Void, [|LLVMTypeRef.Int32|], false) in
     let fn = mdl.AddFunction("print_int", fn_type) in
     let block = LLVMBasicBlockRef.AppendInContext(mdl.Context, fn, $"entry") in
     builder.PositionAtEnd(block)
     let [|value|] = fn.GetParams() in
     builder.BuildCall2(printf_type, printf_fn, [|format_string; value|])
     builder.BuildRetVoid()
     fn_type, fn


let create_fn (builder: LLVMBuilderRef) (mdl: LLVMModuleRef) (name: string) (args: (string * LLVMTypeRef) list) (ret_type: LLVMTypeRef) =
     let names, types = List.unzip(args) in
     let fn_type = LLVMTypeRef.CreateFunction(ret_type, toArray types, false) in
     let fn = mdl.AddFunction(name, fn_type) in
     let params = toList (fn.GetParams()) in
     for name, tmp in zip names params do
         let mutable param = tmp in
         param.Name <- name
     let block = LLVMBasicBlockRef.AppendInContext(mdl.Context, fn, $"entry") in
     builder.PositionAtEnd(block)
     fn_type, fn


let create_update_fn (builder: LLVMBuilderRef) (mdl: LLVMModuleRef) =
     let ptr_type = LLVMTypeRef.CreatePointer(LLVMTypeRef.Int32, 0u) in
     let fn_type, fn = (create_fn builder mdl "update" [
          "src_struct", ptr_type
          "struct_size", LLVMTypeRef.Int32
          "field_offset", LLVMTypeRef.Int32
          "field_value", ptr_type
          "field_size", LLVMTypeRef.Int32
     ] ptr_type) in
     let [|src_struct; struct_size; field_offset; field_value; field_size|] = fn.GetParams() in
     let dst = builder.BuildArrayMalloc(LLVMTypeRef.Int8, struct_size, "dst_struct") in
     LLVM.BuildMemCpy(builder, dst, 4u, src_struct, 4u, struct_size)
     let field_ptr = builder.BuildInBoundsGEP2(LLVMTypeRef.Int8, dst, [|field_offset|]) in
     LLVM.BuildMemCpy(builder, field_ptr, 4u, field_value, 4u, field_size)
     builder.BuildRet(dst)
     fn_type, fn


let create_get_fn (builder: LLVMBuilderRef) (mdl: LLVMModuleRef) =
     let ptr_type = LLVMTypeRef.CreatePointer(LLVMTypeRef.Int32, 0u) in
     let fn_type, fn = (create_fn builder mdl "get" [
          "src_struct", ptr_type
          "field_offset", LLVMTypeRef.Int32
          "field_size", LLVMTypeRef.Int32
     ] ptr_type) in
     let [|src_struct; field_offset; field_size|] = fn.GetParams() in
     let field_ptr = builder.BuildInBoundsGEP2(LLVMTypeRef.Int8, src_struct, [|field_offset|]) in
     let value_ptr = builder.BuildArrayMalloc(LLVMTypeRef.Int8, field_size, "value_ptr") in
     LLVM.BuildMemCpy(builder, value_ptr, 4u, field_ptr, 4u, field_size)
     builder.BuildRet(value_ptr)
     fn_type, fn


let val_id_to_type (id2var_name: Id2VarName) proc =
     let var_name_to_type_name =
          match proc with
          | Procedure(args = args; body = (declarations, _)) ->
               dict ([for declaration in declarations do
                          match declaration with
                          | VarDecl(name, t) -> yield (name, t)
                          | _ -> ()]@args) in
     dict [for item in id2var_name do
               (item.Key, var_name_to_type_name[item.Value])]


let to_llvm (type_descriptions: Types) (builder: LLVMBuilderRef) (mdl: LLVMModuleRef) ast =
     let type_descriptions' = dict [for item in type_descriptions -> item.Key, let def, _, _ = item.Value in def] in
     let mutable fns = Dictionary<string, Tuple<LLVMTypeRef, LLVMValueRef>>() in
     fns["update"] <- create_update_fn builder mdl // segfault if switch lines
     fns["print_int"] <- create_print builder mdl
     fns["get"] <- create_get_fn builder mdl
     for proc in get_procedures ast do
          let fn_name, fn_args, return_type, cfg, numerated_vars, id2var_name = proc_to_ssa type_descriptions' proc in
          let val_id_to_type = val_id_to_type id2var_name proc, type_descriptions in
          let phis = get_phi_defs numerated_vars in
          // printfn "%A" cfg
          let args_type = toArray [for _, t in fn_args -> let _, _, llvm_type = type_descriptions[t] in llvm_type] in
          let _, _, return_type = type_descriptions[return_type] in
          let fn_type = LLVMTypeRef.CreateFunction(return_type, args_type, false) in
          let fn = mdl.AddFunction(fn_name, fn_type) in
          fns[fn_name] <- (fn_type, fn)
          let values = Values() in
          for (id, _), llvm_val in zip fn_args (toList (fn.GetParams())) do
               values[id] <- llvm_val
          let block_ids = unique (flatten [for block in cfg ->
                                                  match block with
                                                  | Cond(block_id = block_id; true_branch_idx = true_branch_idx; false_branch_idx = false_branch_idx) ->
                                                       [block_id; true_branch_idx; false_branch_idx]
                                                  | Statements(block_id = block_id) ->
                                                       [block_id]]) in
          let cfg = if contains -1 block_ids then cfg@[Statements(-1, [Return], -1)] else cfg in
          let blocks = dict [for block_id in block_ids -> (block_id, LLVMBasicBlockRef.AppendInContext(mdl.Context, fn, $"{block_id}"))] in
          let exp_to_llvm' = exp_to_llvm val_id_to_type fns values builder in
          let build_phis' = build_phis builder values val_id_to_type phis in
          for block in cfg do
               match block with
               | Cond(blockId, cond, trueBranchIdx, falseBranchIdx) ->
                    builder.PositionAtEnd(blocks[blockId])
                    build_phis' blockId
                    let cond' = exp_to_llvm' cond in
                    builder.BuildCondBr(cond', blocks[trueBranchIdx], blocks[falseBranchIdx])
                    ()
               | Statements(blockId, statements, nextBlockId) ->
                    builder.PositionAtEnd(blocks[blockId])
                    build_phis' blockId
                    for statement in statements do
                         match statement with
                         | Assignment(Local(id = id), right) ->
                              values[id] <- exp_to_llvm' right
                         | ReturnWithValue(exp) ->
                              builder.BuildRet(exp_to_llvm' exp)
                              ()
                         | Return ->
                              builder.BuildRetVoid()
                              ()
                         | FnCall(name, args) ->
                              let fn_type, fn = fns[name] in
                              let args = toArray [for arg in args -> exp_to_llvm' arg] in
                              builder.BuildCall2(fn_type, fn, args)
                              ()
                    if nextBlockId <> -1 then
                         builder.BuildBr(blocks[nextBlockId])
                         ()
          for item in phis do
               for phi in item.Value do
                    match phi with
                    | Phi(id, _, operands) ->
                         let phi' = values[id] in
                         let one : uint = 1u in
                         for (id, block_id) in operands do
                              phi'.AddIncoming([|values[id]|], [|blocks[block_id]|], one)
     printfn "%A" (mdl.PrintToString())


let process (bin: string) (arg: string) =
    let processStartInfo = new ProcessStartInfo(bin, arg)
    processStartInfo.UseShellExecute <- false
    processStartInfo.CreateNoWindow <- true
    Process.Start(processStartInfo).WaitForExit()


let c_string (s : string) =
     MarshaledString(s.AsSpan()).Value, UIntPtr(uint32 s.Length)


let alignment_in_bits size_in_bytes =
     uint32 ((1 <<< (int (log (float size_in_bytes) / log 2.0))) * 8)

open Microsoft.FSharp.NativeInterop
open LLVMSharp.Interop

let add_debug_information (type_descriptions: Types) (mdl: LLVMModuleRef) ast (path: string) =
     let builder : LLVMDIBuilderRef = mdl.CreateDIBuilder() in
     let file_metadata = builder.CreateFile(Path.GetFileName(path), Path.GetDirectoryName(path)) in
     let compilation_unit = builder.CreateCompileUnit(
          LLVMDWARFSourceLanguage.LLVMDWARFSourceLanguageModula2,
          file_metadata,
          "Modula2", 0, "", 0u, "", LLVMDWARFEmissionKind.LLVMDWARFEmissionFull, 0u, 0, 0, "", ""
     ) in
     let mutable types = Dictionary<string, LLVMMetadataRef>() in
     for item in type_descriptions do
          let def, size_in_bytes, _ = item.Value in
          let metadata =
               match item.Key, def with
               | "", Void ->
                    let name, name_len = c_string "void" in
                    LLVM.DIBuilderCreateBasicType(builder, name, name_len, uint64 0, 0u, LLVMDIFlags.LLVMDIFlagZero)
               | "integer", Int ->
                    let name, name_len = c_string "i32" in
                    LLVM.DIBuilderCreateBasicType(builder, name, name_len, uint64 (size_in_bytes * 8), 0x05u, LLVMDIFlags.LLVMDIFlagZero) // DW_ATE_signed
               // | name, ArrayDef(elems_number, element_type) ->
               //      let subrange : LLVMMetadataRef = LLVM.DIBuilderGetOrCreateSubrange(builder, 0, elems_number) in
               //      let alignment = alignment_in_bits size_in_bytes in
               //      let basic_type = builder.CreateArrayType(
               //                               uint64 (size_in_bytes * 8),
               //                               alignment,
               //                               types[element_type],
               //                               [| subrange |]
               //                          ) in
               //      builder.CreateTypedef(basic_type, name, file_metadata, )
               // | StructDef(FieldsOffsets(fields)) ->

          types[item.Key] <- metadata


let compile (path: string) =
     let folder = System.IO.Path.GetDirectoryName(path) in
     let filename = System.IO.Path.GetFileNameWithoutExtension(path) in
     let path_without_ext = Path.Combine(folder, filename) in
     let mdl = LLVMModuleRef.CreateWithName("aaaa") in
     let builder = LLVMBuilderRef.Create(mdl.Context) in
     let ast = match parse_file path false with Some(ast) -> simplify_ast ast in
     let type_descriptions = get_type_descriptions ast in
     to_llvm type_descriptions builder mdl ast
     let ll = Path.Combine(folder, $"{path_without_ext}.ll") in
     File.WriteAllText(ll, mdl.PrintToString())
     printfn "%A" ll
     process "/home/nikos/llvm-project/build/bin/llc" $"-filetype=obj {ll} -o {path_without_ext}.o"
     process "gcc" $"{path_without_ext}.o -o {path_without_ext} -no-pie"


// /home/nikos/llvm-project/build/bin/clang callgcd.c gcd.llvmir.so -o gcd
// cd /home/nikos/data/compiler_engineering/llvm/ConsoleApp1/compiler_engineering/modula_2
// export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
// ./gcd