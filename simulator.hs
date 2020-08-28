import           Matrix

main :: IO ()
main =
    let m1 = listToMatrix 3 3 [ [(3,2), (5,-1), (0,2)],
                                [(0,0), (12,0), (6,-3)],
                                [(2,0), (4,4), (9,3)] ]
        m2 = listToMatrix 3 3 [ [(1,0), (3,4), (5,-7)],
                                [(10,2), (6,0), (2,5)],
                                [(0,0), (1,0), (2,9)] ]
        m3 = listToMatrix 4 1 [[(-1,0)],[(2,0)]]
    in
    putStr $ getMatrixStr $ tensorMatrices m1 m2
