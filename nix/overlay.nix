final:
  previous:
    with final.haskell.lib;
    {
      wolfPackages =
            # Turn off testing for now
            let wolfPkg = name:
                (dontCheck (failOnAllWarnings (final.haskellPackages.callCabal2nix name (../. + "/${name}") {})));
              in final.lib.genAttrs [
              "wolf-api"
              "wolf-cli"
              "wolf-client"
              "wolf-cub"
              "wolf-data"
              "wolf-data-baked"
              # "wolf-google"
              "wolf-mutt"
              "wolf-server"
              "wolf-test-utils"
              "wolf-web-server"
            ] wolfPkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
        self: super: 
          let
            typedUuidRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "typed-uuid";
              rev = "155c9ec880ca1c12f7dd8a8468b3626de8164823";
              sha256 = "0wvdj07vhd7q93f7sdg4mq8f9nk4w3fjsq3z7nx7zm5dv0j78iwb";
            };
            typedUuidPkg = name:
                      super.callCabal2nix name (typedUuidRepo + "/${name}") {};
            waiGitHttpRepo = final.fetchFromGitHub {
              owner = "NorfairKing";
              repo = "wai-git-http";
              rev = "6da3d87d9227e233d351e1df935a92171551e3ef";
              sha256 = "11fi35zjn5n33m5pr3rpg2b4686sr7p62hbqczvv1c78fhq2l0gs";
            };
            # Turn off testing for now
            waiGitHttpPkg = dontCheck (super.callCabal2nix "wai-git-http" waiGitHttpRepo {});

          in
            final.wolfPackages //
            final.lib.genAttrs [
              "typed-uuid"
              "genvalidity-typed-uuid"
            ] typedUuidPkg //
            {
              wai-git-http = waiGitHttpPkg;
              cautious = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "cautious" "0.3.0.0" {});
              cautious-gen = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "cautious-gen" "0.0.0.0" {});
              sockaddr = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "sockaddr" "0.0.0" {});
            }
        );
      });
    }
