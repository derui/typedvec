let () =
  Alcotest.run "typedvec"
    [
      ("Util", Test_util.tests);
      ("vector", Test_vector.tests);
      ("Size", Test_size.tests);
      ("Quaternion", Test_quaternion.tests);
      ("Ppx", Test_ppx.tests);
      ("Matrix", Test_matrix.tests);
      ("Line", Test_line.tests);
      ("Algebra", Test_algebra.tests);
    ]
