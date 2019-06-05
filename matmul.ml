let matmul_sub : (x:int) -> (y:int) -> (z:int) -> ▷ (Matrix x y) -> ▷ (Matrix y z) -> ▷ (Matrix x z) -> int -> int -> int -> ▷ unit =
  fun x y z m1 m2 mout i j k ->
    if i == x-1 && j == y-1 && k == z-1
    then
      ▶ mout[i][j] = m1[i][k] * m2[k][j]
    else
      let kk = (k+1) % z in
      let jj = if kk == 0 then (j+1) % y else j in
      let ii = if jj == 0 then (i+1) % x else i in
      ▶ (
        mout[i][j] = m1[i][k] * m2[k][j];
        ◀ matmul_sub x y z m1 m2 mout ii jj kk
      )

let matmul : (x:int) -> (y:int) -> (z:int) -> ▷ ((Matrix %x %y) -> (Matrix %y %z) -> (Matrix %x %z)) =
  fun x y z ->
     ▶ (fun m1 m2 ->
        let mout = Matrix.create_matrix x z in
        ◀ (matmul_sub x y z (▶ m1) (▶ m2) (▶ mout) 0 0 0);
        mout
      )

