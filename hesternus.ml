(* 
 * Copyright (c) 2017 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* Parse GitHub API format dates into CalendarLib *)
let parse_github_date p =
  let open Github_t in
  match Ptime.of_rfc3339 p with
  | Error err -> raise (Failure "invalid timestamp FIXME pp_rfc3339_error")
  | Ok (tm,_,_) -> CalendarLib.Date.from_unixfloat (Ptime.to_float_s tm)

let to_github_date d =
  CalendarLib.Printer.DatePrinter.sprint "%F" d

let sort_repo_issue_event a b =
  let open Github_t in
  let aa = parse_github_date a.repo_issue_event_created_at in
  let bb = parse_github_date b.repo_issue_event_created_at in
  CalendarLib.Date.compare aa bb

let date_between a b d =
  match CalendarLib.Date.compare d a with
  |1 -> begin
    match CalendarLib.Date.compare d b with
    | 1 -> false
    | _ -> true
  end
  |_ -> false

(* Track references for Markdown output *) 
let md_refs = Hashtbl.create 7
let add_pr user repo pr =
  let open Github_t in
  let s = Fmt.strf "%s/%s#%d" user repo pr.issue_number in
  Hashtbl.add md_refs s pr.issue_html_url;
  s

let add_user login =
  let open Github_t in
  let s = Fmt.strf "@%s" login in
  let url = Fmt.strf "https://github.com/%s" login in
  Hashtbl.replace md_refs s url;
  s

let add_issue user repo i =
  let open Github_t in
  let s = Fmt.strf "%s/%s#%d" user repo i.issue_number in
  Hashtbl.replace md_refs s i.issue_html_url;
  s

let add_release user repo r =
  let open Github_t in
  let s = Fmt.strf "%s/%s:%s" user repo r.release_tag_name in
  Hashtbl.replace md_refs s r.release_html_url;
  s

let add_repo user repo =
  let s = Fmt.strf "%s/%s" user repo in
  let url = Fmt.strf "https://github.com/%s/%s" user repo in
  Hashtbl.replace md_refs s url;
  s

let print_md_refs () =
  let ks = Hashtbl.fold (fun k v a -> k::a) md_refs [] |> List.sort compare in
  List.iter (fun k ->
    Fmt.pr "[%s]: %s\n" k (Hashtbl.find md_refs k)
  ) ks;
  Fmt.pr "\n%!"

let get_issue_events token (da,db) user repo i =
  let open Github in
  let open Monad in
  let open Github_t in
  Issue.events ~token ~user ~repo ~num:i.issue_number () |> 
  Stream.to_list >>= fun ev ->
  (* Filter events out of the range *)
  List.filter (fun e ->
    let created = parse_github_date e.repo_issue_event_created_at in
    date_between da db created
  ) ev |>
  List.sort sort_repo_issue_event |>
  List.rev |> fun r ->
  return r

let get_issue_activity token (da,db) user repo i =
  let open Github in
  let open Monad in
  let open Github_t in
  let since = to_github_date da in
  let comments = Issue.comments ~token ~since ~user ~repo ~num:i.issue_number  () in
  Stream.to_list comments >>= fun comments ->
  let users = List.fold_left (fun a {issue_comment_user} ->
    let b = issue_comment_user.user_login in if List.mem b a then a else b::a) [] comments |>
    List.map add_user in
  let num_comments = List.length comments in
  return (`Activity (users, num_comments))
 
(* Classify a PR into something that happened to it *) 
let classify_pr (token:Github.Token.t) (da,db) user repo i =
  let open Github in
  let open Monad in
  let open Github_t in
  Fmt.(epr "%a %s/%s#%d %s %s\n%!" (styled `Yellow string) "PR" user repo i.issue_number i.issue_title i.issue_html_url);
  get_issue_events token (da,db) user repo i >>= fun ev ->
  let stuff = List.map (fun {repo_issue_event_event} -> repo_issue_event_event) ev in
  let was_closed = List.mem `Closed stuff in
  let was_merged = List.mem `Merged stuff in
  let was_reopened = List.mem `Reopened stuff in
  (* Classify the PR to find out what happened to it in the time range *)
  (if was_merged then return `Merged else
    if was_closed then return `Closed else
    if was_reopened then return `Reopened else
    get_issue_activity token (da,db) user repo i) >>= fun cla ->
  return (i, cla, ev)

let classify_issue token (da,db) user repo i =
  let open Github in
  let open Monad in
  let open Github_t in
  Fmt.(epr "%a %s/%s#%d %s %s\n%!" (styled `Green string) "Issue" user repo i.issue_number i.issue_title i.issue_html_url);
  get_issue_events token (da,db) user repo i >>= fun ev ->
  let was_newly_opened =
    let cd = parse_github_date i.issue_created_at in
    CalendarLib.Date.compare da cd >= 0 in
  let stuff = List.map (fun {repo_issue_event_event} -> repo_issue_event_event) ev in
  let was_closed = List.mem `Closed stuff in
  (if was_newly_opened && was_closed then
    return `New_and_closed
   else if was_closed then
    return `Closed
   else get_issue_activity token (da,db) user repo i) >>= fun cl ->
  return (i, cl, ev)

let get_actors_from_event ?(init=[]) evs =
  let open Github_t in
  List.fold_left (fun a e ->
    match e.repo_issue_event_actor with
    |None -> a
    |Some user ->
      let l = add_user user.linked_user_login in
      if List.mem l a then a else l::a
  ) init evs |> fun l ->
  String.concat " " (List.map (Fmt.strf "[%s]") l)

let classify_issues_for_repo (token:Github.Token.t) (da,db) (user,repo) =
  Lwt_main.run (Github.(Monad.(run (
    let a = to_github_date da in
    let b = to_github_date db in
    let qualifiers = [`Repo (Fmt.strf "%s/%s" user repo); `Updated (`Range ((Some a),(Some b)))] in
    let keywords = [] in
    let opam_repo_search = Search.issues ~token ~qualifiers ~keywords () in
    Stream.fold (fun a b -> return (a @ b.Github_t.repository_issue_search_items)) [] opam_repo_search >>= fun issues ->
    (* Break up the issues into PRs and bugs *)
    let is_pr i = i.Github_t.issue_pull_request <> None in
    let prs, issues = List.partition is_pr issues in
    (* Iterate over the PRs and issues for the printer *)
    let rec iter fn acc = function [] -> return acc | hd::tl -> fn hd >>= fun r -> iter fn (r::acc) tl in
    iter (classify_pr token (da,db) user repo) [] prs >>= fun prs ->
    iter (classify_issue token (da,db) user repo) [] issues >>= fun issues ->
    let prs = List.map (fun (i,cla,ev) -> (user,repo,i,cla,ev)) prs in
    let issues = List.map (fun (i,cla,ev) -> (user,repo,i,cla,ev)) issues in
    return (prs, issues)
  ))))

let get_releases_for_repo token (da,db) (user,repo) =
  Lwt_main.run (Github.(Monad.(run (
    let open Github_t in
    Release.for_repo ~token ~user ~repo () |> Stream.to_list >>= fun releases ->
    List.filter (fun r ->
      match parse_github_date r.release_published_at with
      | date -> CalendarLib.Date.compare date da >= 0 && (CalendarLib.Date.compare date db < 0)
      | exception _ -> false
    ) releases |> fun releases ->
    return releases
  ))))

let get_all_releases token (da,db) repos =
  List.map (fun (user,repo) ->
    get_releases_for_repo token (da,db) (user,repo) |>
    List.map (fun r -> (user,repo,r))
  ) repos |> List.flatten

let print_releases releases =
  if List.length releases = 0 then () else begin
  Fmt.pr "## Releases this week\n\n";
  List.iter (fun (user,repo,r) ->
    let open Github_t in
    let s = add_release user repo r in
    Fmt.pr "- [%s]\n" s
  ) releases;
  Fmt.pr "\n";
  end

let print_prs i =
    let open Github_t in
    let merged = List.filter (fun (u,r,i,cla,ev) -> cla = `Merged) i in
    if List.length merged > 0 then begin
      Fmt.pr "### PRs merged this week\n\n";
      List.iter (fun (u,r,i,_,ev) ->
        let pr = add_pr u r i in
        let a = get_actors_from_event ev in
        Fmt.pr "- [%s] %s (%s)\n%!" pr i.issue_title a
      ) merged;
      Fmt.pr "\n%!";
    end;
    let closed = List.filter (fun (u,r,i,cla,ev) -> cla = `Closed) i in
    if List.length closed > 0 then begin
      Fmt.pr "### PRs closed this week without merge\n\n";
      List.iter (fun (u,r,i,_,ev) ->
        let pr = add_pr u r i in
        let a = get_actors_from_event ev in
        Fmt.pr "- [%s] %s (%s)\n%!" pr i.issue_title a
      ) closed;
      Fmt.pr "\n%!";
    end;
    let boring = List.filter (fun (u,r,i,cla,ev) -> match cla with `Activity _ -> true |_ -> false) i in
    if List.length boring > 0 then begin
      Fmt.pr "### PRs with activity\n\n";
      List.iter (fun (u,r,i,cla,ev) ->
        let users, num_comments = match cla with `Activity a -> a |_->assert false in
        let pr = add_pr u r i in
        let a = get_actors_from_event ~init:users ev in
        Fmt.pr "- [%s] %s (%s)%s\n%!" pr i.issue_title a
          (match num_comments with 
           |0 -> "" |n -> Fmt.strf " with %d comments" num_comments);
      ) boring;
      Fmt.pr "\n%!";
    end

let compare_issue (u,r,i,cla,ev) (u',r',i',cla',ev') =
  let (++) x fn =
    match x with
    | 0 -> fn ()
    | r -> r in
  let cla_to_int = function `Closed -> 0 | `New_and_closed -> 1 |`Activity (_,n) -> 2+n in
  let cmp_cla a b = compare (cla_to_int a) (cla_to_int b) in
  cmp_cla cla cla' ++ fun () ->
  compare u u' ++ fun () ->
  compare r r' ++ fun () ->
  compare i i' 

let print_issues i =
  let open Github_t in
  Fmt.pr "## Issues of interest\n\n";
  List.sort compare_issue i |> fun issues ->
  List.iter (fun (u,r,i,cla,ev) ->
    let s = add_issue u r i in
    let init = match cla with |`Activity (u,_) -> u |_ -> [add_user i.issue_user.user_login] in
    let a = get_actors_from_event ~init ev in
    let cla =
      match cla with
      | `New_and_closed -> "was opened and closed"
      | `Closed -> "was closed"
      | `Activity (_,num) -> Fmt.strf "had %d event%s" num (if num > 1 then "s" else "")
    in
    Fmt.pr "- [%s] %s %s (%s)\n" s i.issue_title cla a;
  ) issues;
  Fmt.pr "\n"

let print_intro repos =
  (* TODO do an English concat with "and" *)
  let rs = String.concat " " (List.map (fun (u,r) -> add_repo u r) repos) in
  Fmt.pr "This report covers interesting weekly developments in %s.\n\nFILLME\n\n" rs

let print_footer repos =
  let user,repo = List.hd repos in
  Fmt.pr "## External Links or Blogs\n\nFILLME\n\nOther reports in this series can be accessed in [%s/%s](https://github.com/%s/%s/tree/master/reports/\n\n" user repo user repo
  
let run (token:Github.Token.t) repos week year =
  (* FIXME week+1 is because the Internet assigns Jan2nd as week 1 *)
  let (a,b) = CalendarLib.Date.week_first_last (week+1) year in
  Fmt.epr "Retrieving releases from %s -> %s\n%!" (to_github_date a) (to_github_date b);
  let releases = get_all_releases token (a,b) repos in
  Fmt.epr "Retrieving PRs from %s -> %s\n%!" (to_github_date a) (to_github_date b);
  let prs, issues =
    List.map (classify_issues_for_repo token (a,b)) repos |> List.split |> fun (prs,issues) ->
    let issues = List.flatten issues in
    let prs = List.flatten prs in
    (prs,issues) in
  Fmt.pr "\n\n# Weekly dev report for %s to %s (week %d)\n\n" (to_github_date a) (to_github_date b) week;
  print_intro repos;
  print_releases releases;
  print_prs prs;
  print_issues issues;
  print_footer repos;
  print_md_refs ()

open Cmdliner

let week_term =
  let parse s =
    match int_of_string s with
    | week when week > 0 && week <= 52 -> `Ok week
    | week -> `Error (Fmt.strf "'%s' is not a valid integer week (1-52)" s)
    | exception exn -> `Error (Fmt.strf "'%s' is not a valid integer week (1-52)" s)
  in
  let print ppf s = Fmt.pf ppf "%d" s in
  parse, print

(* FIXME query current time *)
let default_year = 2017
let default_week = 1
let todays_date = CalendarLib.Date.from_unixfloat (Unix.gettimeofday ())

let cmd =
  let cookie = Jar_cli.cookie () in
  let doc = "Development report generator from GitHub projects" in
  let week =
    let doc = "Week number of the year to output PR information for (1-52)" in
    Arg.(value & opt week_term default_week & info ["w";"week"] ~docv:"WEEK" ~doc)
  in
  let year =
    let doc = "Year of PR" in
    Arg.(value & opt int default_year & info ["y";"year"] ~docv:"YEAR" ~doc)
  in
  let repos =
    let doc = "GitHub repositories to query (user/repo). The first repository in the list will be considered the main one where the reports will be committed to." in
    Arg.(non_empty & pos_all (pair ~sep:'/' string string) [] & info [] ~docv:"REPOSITORIES" ~doc)
  in
  let man = [
    `S "DESCRIPTION";
    `S "BUGS";
    `P "Report them via e-mail to <anil@recoil.org>, or \
        on the issue tracker at <https://github.com/avsm/hesternus/issues>";
  ] in
  Term.(pure run $ cookie $ repos $ week $ year),
  Term.info "hesternus" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

