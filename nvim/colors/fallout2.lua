vim.cmd("hi clear")
if vim.fn.exists("syntax_on") == 1 then
  vim.cmd("syntax reset")
end

vim.g.colors_name = "fallout2"
vim.o.background = "dark"
vim.o.termguicolors = true

local p = {
  bg = "#383838",
  bg_dark = "#050c01",
  bg_alt = "#1C1C1C",
  bg_sel = "#1C1C1C",
  border = "#789478",

  fg = "#49a12e",
  fg_soft = "#3CF800",
  fg_dim = "#A0A0A0",
  comment = "#9cac9c",
  white = "#ececec",
  gray = "#A0A0A0",

  yellow = "#907824",
  yellow2 = "#e1d949",

  orange = "#eea059",
  orange2 = "#c460a8",

  red = "#C30100",
  red_dark = "#900000",

  green = "#49a12e",
  green_soft = "#3CF800",
  green_dark = "#789478",

  blue = "#30588c",
  cyan = "#406c8c",
  cyan2 = "#9cac9c",
  magenta = "#c460a8",
  magenta_bright = "#fcb0f0",
}

local function hi(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

-- Base UI
hi("Normal", { fg = p.fg, bg = p.bg })
hi("NormalNC", { fg = p.fg, bg = p.bg })
hi("NormalFloat", { fg = p.fg, bg = p.bg_alt })
hi("FloatBorder", { fg = p.orange2, bg = p.bg_alt })
hi("FloatTitle", { fg = p.yellow, bg = p.bg_alt, bold = true })

hi("CursorLine", { bg = p.bg_alt })
hi("CursorColumn", { bg = p.bg_alt })
hi("ColorColumn", { bg = p.bg_alt })

hi("LineNr", { fg = p.comment, bg = p.bg })
hi("CursorLineNr", { fg = p.yellow, bg = p.bg_alt, bold = true })

hi("SignColumn", { fg = p.comment, bg = p.bg })
hi("FoldColumn", { fg = p.comment, bg = p.bg })
hi("Folded", { fg = p.fg_dim, bg = p.bg_alt })

hi("VertSplit", { fg = p.border, bg = p.bg })
hi("WinSeparator", { fg = p.border, bg = p.bg })

hi("StatusLine", { fg = p.yellow, bg = p.bg, bold = true })
hi("StatusLineNC", { fg = p.fg_dim, bg = p.bg_dark })

hi("TabLine", { fg = p.fg_dim, bg = p.bg_dark })
hi("TabLineFill", { fg = p.fg_dim, bg = p.bg_dark })
hi("TabLineSel", { fg = p.yellow, bg = p.bg, bold = true })

hi("Pmenu", { fg = p.fg_soft, bg = p.bg_alt })
hi("PmenuSel", { fg = p.yellow, bg = p.bg_sel, bold = true })
hi("PmenuSbar", { bg = p.bg_dark })
hi("PmenuThumb", { bg = p.border })

hi("Visual", { fg = p.yellow, bg = p.bg_sel })
hi("Search", { fg = p.yellow, bg = p.bg_sel, bold = true })
hi("IncSearch", { fg = p.bg, bg = p.yellow, bold = true })
hi("CurSearch", { fg = p.bg, bg = p.yellow, bold = true })
hi("MatchParen", { fg = p.orange, bg = p.bg_sel, bold = true })

hi("NonText", { fg = p.border, bg = p.bg })
hi("Whitespace", { fg = p.border, bg = p.bg })
hi("EndOfBuffer", { fg = p.bg, bg = p.bg })

hi("Directory", { fg = p.cyan, bold = true })
hi("Title", { fg = p.yellow, bold = true })
hi("ErrorMsg", { fg = p.red, bold = true })
hi("WarningMsg", { fg = p.yellow, bold = true })
hi("MoreMsg", { fg = p.green, bold = true })
hi("Question", { fg = p.yellow, bold = true })

-- Syntax
hi("Comment", { fg = p.comment, italic = true })

hi("Constant", { fg = p.red })
hi("String", { fg = p.fg_soft })
hi("Character", { fg = p.fg_soft })
hi("Number", { fg = p.orange })
hi("Boolean", { fg = p.orange })
hi("Float", { fg = p.orange })

hi("Identifier", { fg = p.gray })
hi("Function", { fg = p.green, bold = true })

hi("Statement", { fg = p.yellow, bold = true })
hi("Conditional", { fg = p.yellow, bold = true })
hi("Repeat", { fg = p.yellow, bold = true })
hi("Label", { fg = p.yellow2 })
hi("Operator", { fg = p.orange2 })
hi("Keyword", { fg = p.yellow, bold = true })
hi("Exception", { fg = p.red })

hi("PreProc", { fg = p.orange })
hi("Include", { fg = p.yellow2 })
hi("Define", { fg = p.orange })
hi("Macro", { fg = p.orange })

hi("Type", { fg = p.cyan })
hi("StorageClass", { fg = p.cyan })
hi("Structure", { fg = p.cyan })
hi("Typedef", { fg = p.orange })

hi("Special", { fg = p.cyan })
hi("SpecialChar", { fg = p.cyan })
hi("Delimiter", { fg = p.gray })
hi("SpecialComment", { fg = p.comment, italic = true })
hi("Tag", { fg = p.orange })

-- Diff / diagnostics
hi("DiffAdd", { fg = p.green, bg = p.bg_alt })
hi("DiffChange", { fg = p.yellow, bg = p.bg_alt })
hi("DiffDelete", { fg = p.red, bg = p.bg_alt })
hi("DiffText", { fg = p.orange, bg = p.bg_sel, bold = true })

hi("DiagnosticError", { fg = p.red })
hi("DiagnosticWarn", { fg = p.yellow })
hi("DiagnosticInfo", { fg = p.blue })
hi("DiagnosticHint", { fg = p.cyan })
hi("DiagnosticOk", { fg = p.green })

hi("DiagnosticUnderlineError", { undercurl = true, sp = p.red })
hi("DiagnosticUnderlineWarn", { undercurl = true, sp = p.yellow })
hi("DiagnosticUnderlineInfo", { undercurl = true, sp = p.blue })
hi("DiagnosticUnderlineHint", { undercurl = true, sp = p.cyan })

-- Treesitter
hi("@comment", { link = "Comment" })
hi("@string", { link = "String" })
hi("@string.regex", { fg = p.cyan })
hi("@number", { link = "Number" })
hi("@boolean", { link = "Boolean" })
hi("@constant", { link = "Constant" })
hi("@constant.builtin", { fg = p.orange })
hi("@keyword", { link = "Keyword" })
hi("@keyword.function", { fg = p.yellow, bold = true })
hi("@function", { link = "Function" })
hi("@function.builtin", { fg = p.yellow2 })
hi("@constructor", { fg = p.orange })
hi("@type", { link = "Type" })
hi("@type.builtin", { fg = p.cyan })
hi("@variable", { fg = p.gray })
hi("@variable.builtin", { fg = p.orange2 })
hi("@property", { fg = p.fg })
hi("@field", { fg = p.fg })
hi("@parameter", { fg = p.white })
hi("@punctuation", { fg = p.gray })
hi("@punctuation.delimiter", { fg = p.gray })
hi("@tag", { fg = p.orange })
hi("@tag.attribute", { fg = p.yellow2 })
hi("@tag.delimiter", { fg = p.gray })

-- LazyVim ecosystem groups
hi("LazyNormal", { fg = p.fg, bg = p.bg })
hi("MasonNormal", { fg = p.fg, bg = p.bg_alt })

hi("TelescopeNormal", { fg = p.fg, bg = p.bg_alt })
hi("TelescopeBorder", { fg = p.orange2, bg = p.bg_alt })
hi("TelescopeTitle", { fg = p.yellow, bg = p.bg_alt, bold = true })
hi("TelescopePromptNormal", { fg = p.yellow, bg = p.bg_dark })
hi("TelescopePromptBorder", { fg = p.yellow2, bg = p.bg_dark })
hi("TelescopePromptTitle", { fg = p.bg, bg = p.yellow, bold = true })
hi("TelescopePreviewTitle", { fg = p.bg, bg = p.orange, bold = true })
hi("TelescopeResultsTitle", { fg = p.bg, bg = p.green, bold = true })
hi("TelescopeSelection", { fg = p.yellow, bg = p.bg_sel, bold = true })
hi("TelescopeMatching", { fg = p.orange, bold = true })

hi("WhichKey", { fg = p.yellow })
hi("WhichKeyGroup", { fg = p.orange })
hi("WhichKeyDesc", { fg = p.fg_soft })
hi("WhichKeySeperator", { fg = p.border })
hi("WhichKeySeparator", { fg = p.border })
hi("WhichKeyFloat", { bg = p.bg_alt })
hi("WhichKeyBorder", { fg = p.orange2, bg = p.bg_alt })

hi("NeoTreeNormal", { fg = p.fg, bg = p.bg_alt })
hi("NeoTreeNormalNC", { fg = p.fg, bg = p.bg_alt })
hi("NeoTreeFloatBorder", { fg = p.orange2, bg = p.bg_alt })
hi("NeoTreeFloatTitle", { fg = p.yellow, bg = p.bg_alt, bold = true })
hi("NeoTreeTitleBar", { fg = p.bg, bg = p.yellow, bold = true })
hi("NeoTreeDirectoryName", { fg = p.cyan })
hi("NeoTreeDirectoryIcon", { fg = p.orange })
hi("NeoTreeFileName", { fg = p.fg })
hi("NeoTreeFileNameOpened", { fg = p.yellow })
hi("NeoTreeRootName", { fg = p.orange, bold = true })
hi("NeoTreeIndentMarker", { fg = p.border })
hi("NeoTreeGitAdded", { fg = p.green })
hi("NeoTreeGitModified", { fg = p.yellow })
hi("NeoTreeGitDeleted", { fg = p.red })

hi("SnacksIndent", { fg = p.border })
hi("SnacksIndentScope", { fg = p.orange2 })

hi("CmpItemAbbr", { fg = p.fg_soft })
hi("CmpItemAbbrMatch", { fg = p.yellow, bold = true })
hi("CmpItemAbbrMatchFuzzy", { fg = p.orange, bold = true })
hi("CmpItemMenu", { fg = p.comment })
hi("CmpItemKind", { fg = p.orange2 })
hi("CmpItemKindFunction", { fg = p.green })
hi("CmpItemKindMethod", { fg = p.green })
hi("CmpItemKindVariable", { fg = p.gray })
hi("CmpItemKindKeyword", { fg = p.yellow })
hi("CmpItemKindClass", { fg = p.magenta_bright })
hi("CmpItemKindModule", { fg = p.cyan })

hi("GitSignsAdd", { fg = p.green })
hi("GitSignsChange", { fg = p.yellow })
hi("GitSignsDelete", { fg = p.red })
