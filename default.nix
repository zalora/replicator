{ cabal, ConfigFile, filepath, Glob, hflags, lensFamilyCore, MissingH
, monadParallel, mtl, pipes, pipesBytestring, pipesGroup, pipesSafe
, pipesShell, pipesZlib, rawStringsQq, regexApplicative, time
, transformers
}:

cabal.mkDerivation (self: {
  pname = "replicator";
  version = "git";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    ConfigFile filepath Glob hflags lensFamilyCore MissingH monadParallel
    mtl pipes pipesBytestring pipesGroup pipesSafe pipesShell pipesZlib
    rawStringsQq regexApplicative time transformers
  ];
  meta = {
    description = "Automate creating MySQL multi-source slaves";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
