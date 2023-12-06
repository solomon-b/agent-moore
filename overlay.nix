final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
      (hfinal: hprev: {
        agent-moore = hfinal.callCabal2nix "agent-moore" (./.) { };
        bifunctors = hfinal.bifunctors_5_6_1;
        semigroupoids = hfinal.semigroupoids_6_0_0_1;
      });
  });
}
