module A = Typedvec.Algebra
module S = Typedvec.Size
module V = A.Vec
module L = Typedvec.Algebra.Line

let tests =
  [
    ( "Line should detect collision 2D lines",
      `Quick,
      fun () ->
        let a_a = V.make S.two 0.0 in
        let a_b = V.make S.two 0.0 in
        let b_a = V.make S.two 0.0 in
        let b_b = V.make S.two 0.0 in
        V.set ~index:0 ~v:2.0 a_b;
        V.set ~index:1 ~v:2.0 a_b;
        V.set ~index:1 ~v:2.0 b_a;
        V.set ~index:0 ~v:2.0 b_b;
        let la = (a_a, a_b) and lb = (b_a, b_b) in
        let detect = L.detect_collision ~a:la ~b:lb () in
        match detect with
        | None        -> failwith "Not detect"
        | Some (a, b) ->
            let a = V.to_list a and b = V.to_list b in
            Alcotest.(check bool) "detected" true (List.nth a 0 -. 1.0 < 0.00001);
            Alcotest.(check bool) "detected" true (List.nth a 1 -. 1.0 < 0.00001);
            Alcotest.(check bool) "detected" true (List.nth b 0 -. 1.0 < 0.00001);
            Alcotest.(check bool) "detected" true (List.nth b 1 -. 1.0 < 0.00001) );
  ]
