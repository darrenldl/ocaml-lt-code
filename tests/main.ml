module Qc = struct
  let encode_decode =
    QCheck.Test.make ~count:10 ~name:"encode_decode"
    QCheck.(triple bool (array_of_size (Gen.int_range 1 100) (string_of_size (Gen.return 10))) (int_bound 200))
    (fun (systematic, data_blocks, drop_count_offset) ->
      QCheck.assume (Array.length data_blocks > 0);
      let data_blocks = Array.map Bytes.of_string data_blocks in
      let drop_count = Array.length data_blocks + drop_count_offset in
      match Ofountain.encode ~systematic ~drop_count data_blocks with
      | Error _ -> false
      | Ok (ctx, drops) ->
          match Ofountain.decode ctx (Ofountain.Drop_set.of_seq @@ Array.to_seq drops) with
          | Error _ -> false
          | Ok data_blocks' ->
              let (_, has_mismatch) =
                Array.fold_left (fun (i, has_mismatch) data' ->
                  let has_mismatch = has_mismatch || not (Bytes.equal data' data_blocks.(i)) in
                  (succ i, has_mismatch)
                  )
                (0, false)
                data_blocks'
                  in
                  not has_mismatch
    )
end

let () =
  let alco_suites =
    [
    ]
  in
  let qc_suites =
    [
      ("encode_decode", [Qc.encode_decode]);
    ]
    |> List.map (fun (name, test) ->
           (name, List.map QCheck_alcotest.to_alcotest test))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "ofountain" suites
