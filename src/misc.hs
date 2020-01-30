{-
  -- 7...
  --let scambleSequence = [R,U,L,R,B,L',U'] -- (track expanded: 2120 steps, 1.7s)

  -- 8..
  --let scambleSequence = [R,U,L,R,B,L',U',F] -- (track expanded 20785 steps, 3m25)

  -- dont track expanded...
  --let scambleSequence = [R,U,L,R,B,L',U',F] -- 26025 steps, (see 100: 4.7s) (see 1000; 1.8s)

  -- 9..
  let scambleSequence = [R,U,L,R,B,L',U',F,D] -- 453730 steps, 1m40
  -- only view every 100,000 steps -> 26s
  -- ok, so counting the frontier size is expensive.
  -- but nice to know, so instead lets track it exlicitly
  -- -> 18s
  -- -> 17s (remove expanded/frontierSize entirely)
  -- -> 15s (some strictness annotations)

  -- 10.. nope!! blows the fans!
  --let scambleSequence = [R,U,L,R,B,L',U',F,D,F]

  let scrambledState = foldl applyMove solvedState scambleSequence
  putStrLn $ unlines (_prettyState scrambledState)
  print (distance solvedState scrambledState)
  let links = _allMoves
  (i,sol) <- viewSearch (mkGraph links) scrambledState
-}
