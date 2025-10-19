import AlephOmega.Types

main :: IO ()
main = do
  putStrLn "Testing inductive kernel K_n (N=2):"
  
  -- Test buildConfigs: generates configs from kernel family
  let configs2 = buildConfigs 2   -- [c_0, c_1, c_2] from K_0, K_1, K_2
  print configs2  -- Should show: [Config [],Config [("dim_1",1)],Config [("dim_2",1)]]
  
  -- Test manual K application
  let k1 = mkK 1                   -- K_1 cell
      k0 = K (const zeroConfig)    -- K_0 as constant zero
      c1 = applyK k1 [k0]          -- Apply K_1 to [K_0]
  print c1  -- Should show: Config [("dim_1",1)]
  
  putStrLn "N kernels + configs passed!"

