let () =
  let x = ref 0 in
  if true then: incr x; incr x;
  assert (!x = 2)

