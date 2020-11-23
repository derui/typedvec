let () =
  Alcotest.run "typedvec"
    [
      ("Util", Test_util.tests);
      ("vector", Test_vector.tests);
      ("Size", Test_size.tests);
      ("Quaternion", Test_quaternion.tests);
    ]
