module RAL exposing (..)
--import MapAccumulate exposing (..)

type Tree a = Leaf a | T (Tree a) a (Tree a)

type RAList a = RAList (List (Tree a, Int))

--construct empty RAL
empty : RAList a
empty =
    RAList []

--implemented Random Access List functions needed to build initial list from 
--this paper - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.55.5156&rep=rep1&type=pdf
get : Int -> RAList a -> a
get i ral =
    let 
        foo (t, s) ind =
            case ((t,s), ind) of
                ((Leaf x, size), 0) -> 
                    x
                ((Leaf x, size), xxx) ->
                    if ind == xxx then 
                        x
                        --Debug.todo "out of bounds first"
                    else 
                        x
                        --Debug.todo "out of bounds second"
                ((T t1 x t2, size), 0) ->
                    x
                ((T t1 x t2, size), z) ->
                    let 
                        sp = size // 2
                    in
                        if z <= sp then
                            foo (t1, sp) (z-1)
                        else
                            foo (t2, sp) (z-1-sp)
    in 
        case ral of
            (RAList ((t, s)::rest)) ->
                if i <= s then
                    foo (t, s) (i)
                else
                    get (i-s) (RAList rest)
            (RAList []) ->
                Debug.todo "not in list"
            

cons : a -> RAList a -> RAList a
cons x eRal = 
    case eRal of 
        (RAList ((t1, s1)::(t2, s2)::rest)) ->
             let (RAList lp) = eRal in
                if s1 == s2 then
                    RAList(((T t1 x t2), (1+s1+s2))::rest)
                else
                    RAList((Leaf x, 1)::lp)
        _ ->
            let (RAList lp) = eRal in
                RAList((Leaf x, 1)::lp)