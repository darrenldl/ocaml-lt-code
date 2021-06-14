module Qc = struct
  let encode_decode_systematic =
    QCheck.Test.make ~count:10_000 ~name:"encode_decode"
      QCheck.(
        pair
          (array_of_size (Gen.int_range 1 10) (string_of_size (Gen.return 10)))
          (int_bound 20))
      (fun (data_blocks, drop_count_offset) ->
        QCheck.assume (Array.length data_blocks > 0);
        QCheck.assume (Array.for_all (fun x -> String.length x > 0) data_blocks);
        let data_blocks = Array.map Cstruct.of_string data_blocks in
        let drop_count = Array.length data_blocks + drop_count_offset in
        match Ofountain.encode ~systematic:true ~drop_count data_blocks with
        | Error _ -> false
        | Ok (ctx, drops) -> (
            match
              Ofountain.decode ctx
                (Ofountain.Drop_set.of_seq @@ Array.to_seq drops)
            with
            | Error _ -> false
            | Ok data_blocks' ->
                let _, has_mismatch =
                  Array.fold_left
                    (fun (i, has_mismatch) data' ->
                      let has_mismatch =
                        has_mismatch
                        || not (Cstruct.equal data' data_blocks.(i))
                      in
                      (succ i, has_mismatch))
                    (0, false) data_blocks'
                in
                not has_mismatch))
end

let () =
  let alco_suites = [] in
  let qc_suites =
    [ ("encode_decode_systematic", [ Qc.encode_decode_systematic ]) ]
    |> List.map (fun (name, test) ->
           (name, List.map QCheck_alcotest.to_alcotest test))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "ofountain" suites