import Lake
open Lake DSL

package «tests» where
  -- add package configuration options here

lean_lib «Tests» where
  -- add library configuration options here

@[default_target]
lean_exe «tests» where
  root := `Main
