local has_telescope, telescope = pcall(require, "telescope")
if not has_telescope then
  error "This extension requires telescope.nvim (https://github.com/nvim-telescope/telescope.nvim)"
end

local scout_picker = require('telescope._extensions.scout.picker')

return telescope.register_extension {
  exports = {
    scout = function(opts) scout_picker(opts or {}) end,
  }
}
