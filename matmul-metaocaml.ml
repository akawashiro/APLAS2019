(* let matmul_sub : int -> (y:int) -> (z:int) -> (Matrix x y) -> (Matrix y z) -> (Matrix x z) -> int -> int -> int -> ▷ unit =
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
        ◀ (matmul_sub x y z (◀ m1) (◀ m2) (◀ mout) 0 0 0);
        mout
      )
*)

let rec matmul_sub : int -> int -> int -> (int array array code) -> (int array array code) -> (int array array code) -> int -> int -> int -> (unit code) =
  fun x y z m1 m2 mout i j k ->
    if i == x-1 && j == y-1 && k == z-1
    then
      .< (.~(mout)).(i).(j) <- (.~(m1)).(i).(k) * (.~(m2)).(k).(j) >.
    else
      let kk = (k+1) mod z in
      let jj = if kk == 0 then (j+1) mod y else j in
      let ii = if jj == 0 then (i+1) mod x else i in
      let mul = .< (.~(mout)).(i).(j) <- (.~(m1)).(i).(k) * (.~(m2)).(k).(j) >. in
      let cont = matmul_sub x y z m1 m2 mout ii jj kk in
      .< .~mul >. in
      (* Can't we use a semicolun in brackets? *)
      (* .< .~mul; .~cont >. in *)

let matmul : int -> int -> int -> ((int array array) -> (int array array) -> (int array array)) code =
  fun x y z ->
  .<fun m1 m2 ->
    let mout = Array.make_matrix x z 0 in
    .~ (matmul_sub x y z .<m1>. .<m2>. .<mout>. 0 0 0);
    mout >. in

let x = 2 in 
let y = 2 in
let z = 2 in
let m1 = Array.make_matrix x y 1 in
let m2 = Array.make_matrix x y 2 in
let mout = Runcode.run (matmul x y z) m1 m2 in
print_int mout.(0).(0);;
