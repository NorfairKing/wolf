# wolf
Helps you keep track of the important information in your personal and professional life.

CLI-first (personal-focused) CRM.

# Setup Guide

## macOS (with git)

0. Ensure Haskell is installed and set up. Using Homebrew, the following should do the trick:

```bash
brew install haskell-stack && stack setup
```

Note: You can run `stack path` as a sanity check.
By default, the newly set-up `ghc` should be located in `~/.stack/x86_64-osx/ghc-<version>/bin`.

1. Ensure you're on the right branch, since not much is going on on master (yet):

```bash
git fetch && git checkout development
```

2. Install the dependencies.
(This may take about 20 minutes, so feel free to grab yourself a cup of your favorite nutrients while waiting.)


```bash
stack install
```

3.1. If you've already been using wolf on another machine, and have a remote git repo (e.g., a private GitHub repo)
with your data, you can link it to the current wolf installation as follows:

```bash
mkdir -p ~/.wolf && cd $_ && git clone <your-repo> .
```

3.2. If starting from scratch, running
```
wolf init && wolf git init
```
sets up your notes, backed by git. If you don't want to use git, then `wolf init` is enough.

4. Set up shell completion for maximum productivity. Add
```bash
zstyle ':completion:*' rehash true

if $(command -v wolf >/dev/null 2>&1); then
  source <(wolf --bash-completion-script $(which wolf))
fi
```
to your `.bashrc` (or `.zshrc`, etc.). This will allow tab autocompletion on wolf subcommands, names, and other flags. 

For zshell, you may need to add the following before the above snippet for compatibility:
```bash
autoload bashcompinit
bashcompinit
```

5. Howl away!
