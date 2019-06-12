let rec vadd1 n v1 v2 =
  if n = 0
  then 
    .< [] >.
  else
    .< 
    let t1 = List.tl .~v1 in
    let t2 = List.tl .~v2 in
    List.hd (.~v1) + List.hd (.~v2)
    :: .~(vadd1 (n-1) .<t1>. .<t2>.) >.

let rec vadd n =
  .< fun v1 -> fun v2 -> .~(vadd1 n .<v1>. .<v2>.)>.
