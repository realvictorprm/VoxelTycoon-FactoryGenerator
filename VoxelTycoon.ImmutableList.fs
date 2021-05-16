module VoxelTycoon.ListUtils

#nowarn "0077" // Don't warn on usage of `get_Item`

let inline iter fn ls =
    let getItem i = (^a : (member get_Item: int -> ^b)(ls, i))
    let count = (^a: (member Count: int)(ls))
    for i in 0..count - 1 do
        fn <| getItem i
        

