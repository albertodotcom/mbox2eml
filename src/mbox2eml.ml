open Core.Std

let split_by_from ic = In_channel.fold_lines ic ~init:[] ~f:(fun acc line ->
    if Str.string_match (Str.regexp "From ") line 0
    then (line, [line]) :: acc
    else (
      match acc with
      | (hd, lines) :: tl -> (hd, line :: lines) :: tl
      | _ -> failwith "No accumulator with initialised"
    )
  )

let extract_emails_from_mbox filename =
  In_channel.with_file filename ~f:(fun ic -> split_by_from ic)

let write_emls folder = List.iter ~f:(fun (hd, lines) ->
    let open Filename in
    Out_channel.with_file (concat folder (hd ^ ".eml")) ~f:(fun f ->
        List.iter (List.rev lines) ~f:(fun line -> Out_channel.output_string f (line ^ "\n"));
      )
  )

let ensure_dir folder_path =
  match Sys.is_directory ~follow_symlinks:true folder_path with
  | `Yes -> ()
  | _ ->
    print_endline (folder_path ^ " does not exists so I'll make it");
    Unix.mkdir_p folder_path

exception File_not_found of string
let check_file_exists file_path =
  match Sys.is_file file_path with
  | `Yes -> ()
  | _ -> raise (File_not_found (file_path ^ " does not exist"))

let command =
  Command.basic
    ~summary:"Convert mbox to eml"
    ~readme:(fun () -> "This program converts a mbox file into a series of eml files.")
    Command.Spec.(empty +> anon (sequence ("filename" %: file)))
    (fun args () ->
       match args with
       | [mbox ; eml_folder] -> (
           try
             check_file_exists mbox;

             let eml_folder = Filename.concat Filename.current_dir_name eml_folder in
             ensure_dir eml_folder;

             extract_emails_from_mbox mbox
             |> write_emls eml_folder
           with
           | File_not_found file_path_err -> print_endline file_path_err
           | _ -> print_endline "Sorry there was an unexpected error"
         )
       | _ -> print_endline "This command needs 2 arguments: an mbox file and a eml folder"
    )

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
