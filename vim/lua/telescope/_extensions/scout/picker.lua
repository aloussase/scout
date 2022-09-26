vim = vim

local pickers = require "telescope.pickers"
local finders = require "telescope.finders"
local conf = require("telescope.config").values
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"

return function(opts)
  opts = opts or {}

  opts.cwd = opts.cwd and vim.fn.expand(opts.cwd) or vim.loop.cwd()

  local function entry_maker(line)
    if vim.startswith(line, 'Searching') then
      return nil
    end

    local parts = vim.split(line, ',', { plain = true, trimempty = true })
    local data = { name = parts[1], uri = parts[2], description = parts[3] }

    return {
      value = data,
      display = string.format('%s -- %s', data.name, data.description),
      ordinal = data.name,
    }
  end

  local live_scouter = finders.new_job(function(prompt)
    if not prompt or prompt == "" then
      return nil
    end
    return {
      'scout',
      'search',
      prompt,
      '--format',
      'csv',
      '--select',
      'name,uri,description',
    }
  end, entry_maker, opts.max_results, opts.cwd)

  pickers.new(opts, {
    prompt_title = "Scout",
    finder = live_scouter,
    sorter = conf.generic_sorter(opts),
    attach_mappings = function(prompt_bufnr)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local entry = action_state.get_selected_entry()
        local open_command
        if vim.fn.has('mac') then
          open_command = string.format('open %s >/dev/null 2>&1', entry.value.uri)
        elseif vim.fn.has('linux') then
          open_command = string.format('xdg-open %s >/dev/null 2>&1', entry.value.uri)
        else
          vim.notify('OS not supported', vim.log.levels.WARN, { title = 'Scout Warning' })
          return
        end
        local _ = os.execute(open_command)
        if vim.v.shell_error ~= 0 then
          vim.notify(
            string.format('There was a problem running ""', open_command),
            vim.log.levels.ERROR,
            { title = 'Scout Error' }
          )
        end
      end)
      return true
    end,
  }):find()
end
