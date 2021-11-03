# notes-lsp

A language server for taking notes

## Configuration

### Neovim

```lua
local lspconfig = require 'lspconfig'
local util = require 'lspconfig/util'
local configs = require 'lspconfig/configs'

configs.notes_lsp = {
  default_config = {
    cmd = { 'notes-lsp' },
    filetypes = { 'markdown' },
    root_dir = function(fname)
      return util.root_pattern('.git', 'neuron.dhall')(fname)
    end,
  },
}

lspconfig.notes_lsp.setup { }
```