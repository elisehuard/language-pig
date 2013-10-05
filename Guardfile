# ruby script to run build and run tests every time a source or test file is modified
# required: ruby language
# gem install guard guard-shell
guard :shell do
  watch(%r{.*\.cabal$}) do
    `cabal build && cabal test`
  end

  watch(%r{src/.*hs}) do
    `cabal build && cabal test`
  end

  watch(%r{test/.*hs}) do
    `cabal build && cabal test`
  end
end
