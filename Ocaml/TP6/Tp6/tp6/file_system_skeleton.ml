

open List
open String

(*============================= Excercie1 ==============================*)
(*============================= question1 ==============================*)
type tree =
  |Leaf of int
  |Node of char * tree list

let example_tree =
  Node ('a', 
    [Node ('b', [Leaf 1; Leaf 2; Leaf 3]);
      Node ('c', [Node ('d', [Leaf 4; Leaf 5]); Node ('e', [Leaf 6])]);
      Leaf 7;
      Node ('f', [Node ('g', [Leaf 8]); Leaf 9])])


let rec tree_count_nodes tree =
  match tree with
    | Leaf(a) -> 0
    | Node(a, b) -> 1 + tree_count_nodes_aux b
  and tree_count_nodes_aux tree_list =
    List.fold_left (fun x y -> x + (tree_count_nodes y)) 0 tree_list

(*----------------------------------------Q2---------------------------------------------------*)

let tree_count_nodes tree =
  let rec aux lst acc =
      if lst <> [] then
          match hd lst with
          |Leaf value -> aux (tl lst) acc
          |Node (value, l) -> aux (tl lst) (acc+1) + aux l 0
      else
          acc
  in 
      match tree with
          |Leaf value-> 0
          |Node (_,l) -> aux l 1

(*----------------------------------------Q3---------------------------------------------------*)

let tree_list_leaves tree =
  let rec aux lst res = 
      if lst <> [] then
          match hd lst with
              |Leaf value -> aux (tl lst) (value::res)
              |Node (_,l) -> aux (tl lst) ((aux l []) @ res)
      else
          res
  in
      match tree with
          |Leaf value -> value::[]
          |Node (_,l) ->List.rev (aux l [])


(* file system item type *)
type fs_file_type = PDF (* portable document format *)
                  | DOC (* microsoft document *)
                  | PNG (* portable network graphics *)
                  | JPG (* joint photographic experts group, *)
                  | AVI (* audio video interleave *)
                  | MKV (* matroska video *)

(* file system item name alias *)
type fs_item_name = string

(* file system item size alias *)
type fs_file_size = int

(* file system file *)
(* a file is described by its name, its type and its size *)
type fs_file = File of fs_item_name * fs_file_type * fs_file_size

(* file system folder *)
(* a folder is simply described by its name *)
type fs_folder = Folder of fs_item_name

(* file system item *)
(* a file system item is either a folder (containing items) or a file *)
type fs_item = FolderItem of fs_folder * fs_item list
             | FileItem of fs_file



(* val files : fs_item -> fs_file list = <fun> *)

let files fs =
  let rec go l = match l with
  | []                           -> []
  | FileItem(file)::l'           -> file::(go l')
  | FolderItem(folder, l')::l''  -> (go l')@(go l'')
  in go [fs]

(* val folders : fs_item -> fs_folder list = <fun> *)

let folders fs =
  let rec go acc l = match l with
  | []                           -> acc
  | FileItem file::l'            -> go acc l'
  | FolderItem (folder, l')::l'' -> go (go (folder::acc) l') l''
  in go [] [fs]

(* val is_image : fs_file -> bool = <fun> *)

let is_image f = match f with
  | File (_, JPG, _) -> true
  | File (_, PNG, _) -> true
  | _ -> false

(* val is_movie : fs_file -> bool = <fun> *)

let is_movie f = match f with
  | File (_, MKV, _) -> true
  | File (_, AVI, _) -> true
  | _ -> false


(* val is_document : fs_file -> bool = <fun> *)

let is_document f = match f with
  | File (_, PDF, _) -> true
  | File (_, DOC, _) -> true
  | _ -> false

(* val images : fs_item -> fs_file list = <fun> *)

let images fs = List.filter is_image (files fs)

(* val movies : fs_item -> fs_file list = <fun> *)

let movies fs = List.filter is_movie (files fs)

(* val documents : fs_item -> fs_file list = <fun> *)

let documents fs = List.filter is_document (files fs)

let equal s1 s2 = (compare s1 s2) = 0

(* val rec_search_list : fs_file list -> String.t -> fs_file list = <fun> *)

let rec rec_search_list l name = match l with
  | []                         -> []
  | File (n, _, _) as file::l' -> if equal n name
                                  then file::(rec_search_list l') name
                                  else rec_search_list l' name

(* val tail_rec_search_list : fs_file list -> String.t -> fs_file list = <fun> *)

let tail_rec_search_list l name =
  let rec go l acc = match l with
    | []                         -> acc
    | File (n, _, _) as file::l' -> if equal n name
                                    then go l' (file::acc)
                                    else go l' acc
  in go l []

(* val not_rec_search_list : fs_file list -> String.t -> fs_file list = <fun> *)

let not_rec_search_list l name =
  let select (File (n, _, _)) = equal n name
  in List.filter select l

(* val search : fs_item -> String.t -> fs_file list = <fun> *)

let search fs = not_rec_search_list (files fs)

(* val search_documents : fs_item -> String.t -> fs_file list = <fun> *)

let search_documents fs = not_rec_search_list (documents fs)

(* val search_documents_fun : (fs_file list -> 'a) -> fs_item -> 'a = <fun> *)

let search_documents_fun search_fun fs = search_fun (documents fs)

(* val size_images : fs_item -> int = <fun> *)

let size_images fs = fold_left (fun s (File(_, _, s')) -> s+s') 0 (images fs)

(* val fs_filter : (fs_item -> bool) -> fs_item -> fs_item_name list = <fun> *)

let fs_filter p fs =
  let rec go l = match l with
  | [] -> []
  | FileItem (File (n , _, _)) as item::l'  -> let l'' = go l'
                                               in if p item
                                                  then n::l''
                                                  else l''
  | FolderItem (Folder n, l') as item::l'' -> let k = go l' and k' = go l''
                                              in if p item
                                                 then n::(k @ k')
                                                 else k @ k'
  in go [fs]

(* val item_names_with_large6_name : fs_item -> fs_item_name list = <fun> *)

let item_names_with_large6_name =
  let large6 fs =
    let res n = length n >= 6
    in match fs with
      | FolderItem ((Folder n), _)  -> res n
      | FileItem   (File (n, _, _)) -> res n
  in fs_filter large6

(* val item_names_with_digit_in_name : fs_item -> fs_item_name list = <fun> *)

let item_names_with_digit_in_name  =
  let numbers = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
    let digit_in_name fs =
      let res n   = fold_left (fun b c -> b || String.contains n c) false 		numbers
      in match fs with
        | FolderItem ((Folder n), _)  -> res n
        | FileItem   (File (n, _, _)) -> res n
  in fs_filter digit_in_name

(* val full_paths : fs_item -> string list = <fun> *)

let full_paths fs =
  let rec go l acc = match l with
  | []                                -> []
  | (FolderItem ((Folder n),l'))::l'' -> let s = (concat "/" [acc; n])
                                         in s::(go l' s) @ (go l'' acc)
  | (FileItem (File (n, _, _)))::l'   -> (concat "/" [acc; n])::go l' acc
  in go [fs] ""

(* val no_two_identical_names : String.t list -> bool = <fun> *)

let no_two_identical_names l =
  let rec go l' = match l' with
    | []         -> true
    | n::[]      -> true
    | n::n'::l'' -> (String.compare n n' != 0) && go l''
  in go (sort String.compare l)

(* val name_with_ext : fs_item -> fs_item_name = <fun> *)

let name_with_ext fs = match fs with
  | FolderItem (Folder n, _)    -> n
  | FileItem (File (n, PDF, _)) -> n ^ ".pdf"
  | FileItem (File (n, DOC, _)) -> n ^ ".doc"
  | FileItem (File (n, PNG, _)) -> n ^ ".png"
  | FileItem (File (n, JPG, _)) -> n ^ ".jpg"
  | FileItem (File (n, AVI, _)) -> n ^ ".avi"
  | FileItem (File (n, MKV, _)) -> n ^ ".mkv"

(* val check : fs_item -> bool = <fun> *)

let check fs =
  let check_folder l =
    let filenames   = List.map name_with_ext (List.filter is_file l) and
        foldernames = List.map name_with_ext (List.filter is_folder l)
    in no_two_identical_names filenames && no_two_identical_names foldernames
  in
  let rec go fs = match fs with
    | [] -> true
    | (FolderItem (Folder n, l'))::l'' -> check_folder l' && go l' && go l''
    | (FileItem _)::l'                 -> go l'
  in match fs with
  | FolderItem (Folder _, l) as root -> go [root]
  | _                        -> false


(* val du : fs_item -> (fs_item_name * fs_file_size) list = <fun> *)

let du fs =
  let rec go l = match l with
  | [] -> 0,[]
  | FileItem (File (n, t, s))::l'  -> let (s', ldu') = go l' in
                                      (s+s', (concat "." [n; extension t],s)::ldu')
  | FolderItem (Folder n, l')::l'' -> let (s', ldu')   = go l' and
                                          (s'', ldu'') = go l'' in
                                      (s'+s'', (n, s')::ldu'@ldu'')
  in snd (go [fs])



