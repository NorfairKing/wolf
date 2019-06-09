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
              rev = "a34695d908ed56f5ef09357d63c18553a8294e68";
              sha256 = "1xa8c4lgmfr5hqpyry2v7i4b4f8xmpc20y9b3l9sa7ggg93alyjp";
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
              wolf = final.wolfPackages.wolf-cli; # Why is this necessary?
              wai-git-http = waiGitHttpPkg;
              cautious = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "cautious" "0.3.0.0" {});
              cautious-gen = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "cautious-gen" "0.0.0.0" {});
              sockaddr = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "sockaddr" "0.0.0" {});
            }
        );
      });
    }
