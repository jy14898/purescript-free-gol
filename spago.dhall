{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support", "control", "tailrec", "transformers", "identity", "undefined", "lists", "memoize", "variant", "free", "aff", "halogen" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
