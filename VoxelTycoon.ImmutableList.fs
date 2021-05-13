module VoxelTycoon.ListUtils

let inline iter fn ls =
    let getItem i = (^a : (member get_Item: int -> ^b)(ls, i))
    let count = (^a: (member Count: int)(ls))
    for i in 0..count - 1 do
        fn <| getItem i
        

