vim.cmd("hi clear")
if vim.fn.exists("syntax_on") == 1 then
  vim.cmd("syntax reset")
end

vim.g.colors_name = "pico8"
vim.o.background = "dark"
vim.o.termguicolors = true

local p = {
  bg = "#1D2B53",
  bg_dark = "#000000",
  bg_alt = "#5F574F",
  bg_sel = "#FFEC27",
  border = "#83769C",
  bg_line = "#754665",

  fg = "#C2C3C7",
  fg_soft = "#FFF1E8",
  fg_dim = "#83769C",
  comment = "#83769C",
  white = "#FFF1E8",
  gray = "#5F574F",

  red = "#FF004D",
  pink = "#FF77A8",
  orange = "#FFA300",
  yellow = "#FFEC27",
  green = "#00E436",
  blue = "#29ADFF",
  blue_bright = "#065AB5",
  cyan = "#125359",
  purple = "#7E2553",
  peach = "#FFCCAA",
  brown = "#AB5236",
}

local function hi(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

hi("Normal", { fg = p.fg, bg = p.bg })
hi("NormalNC", { fg = p.fg, bg = p.bg })
hi("NormalFloat", { fg = p.fg_soft, bg = p.bg_dark })
hi("FloatBorder", { fg = p.border, bg = p.bg_dark })
hi("FloatTitle", { fg = p.yellow, bg = p.bg_dark, bold = true })

hi("CursorLine", { bg = p.bg_line })
hi("CursorColumn", { bg = p.bg_line })
hi("ColorColumn", { bg = p.bg_line })

hi("LineNr", { fg = p.comment, bg = p.bg })
hi("CursorLineNr", { fg = p.fg, bg = p.bg_line, bold = true })

hi("SignColumn", { fg = p.comment, bg = p.bg })
hi("FoldColumn", { fg = p.comment, bg = p.bg })
hi("Folded", { fg = p.fg_dim, bg = p.bg_dark })

hi("VertSplit", { fg = p.border, bg = p.bg })
hi("WinSeparator", { fg = p.border, bg = p.bg })

hi("StatusLine", { fg = p.white, bg = p.border, bold = true })
hi("StatusLineNC", { fg = p.fg_dim, bg = p.border })

hi("TabLine", { fg = p.white, bg = p.white })
hi("TabLineFill", { fg = p.fg_dim, bg = p.white })
hi("TabLineSel", { fg = p.white, bg = p.pink, bold = true })

hi("Pmenu", { fg = p.white, bg = p.bg_dark })
hi("PmenuSel", { fg = p.bg_dark, bg = p.yellow, bold = true })
hi("PmenuSbar", { bg = p.bg_alt })
hi("PmenuThumb", { bg = p.border })

hi("Visual", { fg = p.bg_dark, bg = p.bg_sel })
hi("Search", { fg = p.bg_dark, bg = p.yellow, bold = true })
hi("IncSearch", { fg = p.white, bg = p.red, bold = true })
hi("CurSearch", { fg = p.white, bg = p.red, bold = true })
hi("MatchParen", { fg = p.orange, bg = p.purple, bold = true })

hi("NonText", { fg = p.border, bg = p.bg })
hi("Whitespace", { fg = p.border, bg = p.bg })
hi("EndOfBuffer", { fg = p.bg, bg = p.bg })

hi("Directory", { fg = p.blue_bright, bold = true })
hi("Title", { fg = p.green, bold = true })
hi("ErrorMsg", { fg = p.red, bold = true })
hi("WarningMsg", { fg = p.yellow, bold = true })
hi("MoreMsg", { fg = p.green, bold = true })
hi("Question", { fg = p.pink, bold = true })

hi("Comment", { fg = p.comment, italic = true })

hi("Constant", { fg = p.blue })
hi("String", { fg = p.blue })
hi("Character", { fg = p.blue })
hi("Number", { fg = p.blue })
hi("Boolean", { fg = p.blue })
hi("Float", { fg = p.blue })

hi("Identifier", { fg = p.peach })
hi("Function", { fg = p.fg, bold = true })

hi("Statement", { fg = p.pink, bold = true })
hi("Conditional", { fg = p.pink, bold = true })
hi("Repeat", { fg = p.pink, bold = true })
hi("Label", { fg = p.orange })
hi("Operator", { fg = p.orange })
hi("Keyword", { fg = p.pink, bold = true })
hi("Exception", { fg = p.red, bold = true })

hi("PreProc", { fg = p.orange })
hi("Include", { fg = p.green, bold = true })
hi("Define", { fg = p.orange })
hi("Macro", { fg = p.orange })

hi("Type", { fg = p.white })
hi("StorageClass", { fg = p.white })
hi("Structure", { fg = p.white })
hi("Typedef", { fg = p.white })

hi("Special", { fg = p.cyan })
hi("SpecialChar", { fg = p.cyan })
hi("Delimiter", { fg = p.fg_dim })
hi("SpecialComment", { fg = p.comment, italic = true })
hi("Tag", { fg = p.orange })

hi("DiffAdd", { fg = p.green, bg = p.bg_dark })
hi("DiffChange", { fg = p.yellow, bg = p.bg_dark })
hi("DiffDelete", { fg = p.red, bg = p.bg_dark })
hi("DiffText", { fg = p.white, bg = p.purple, bold = true })

hi("DiagnosticError", { fg = p.red })
hi("DiagnosticWarn", { fg = p.yellow })
hi("DiagnosticInfo", { fg = p.blue_bright })
hi("DiagnosticHint", { fg = p.cyan })
hi("DiagnosticOk", { fg = p.green })

hi("DiagnosticUnderlineError", { undercurl = true, sp = p.red })
hi("DiagnosticUnderlineWarn", { undercurl = true, sp = p.yellow })
hi("DiagnosticUnderlineInfo", { undercurl = true, sp = p.blue_bright })
hi("DiagnosticUnderlineHint", { undercurl = true, sp = p.cyan })

hi("@comment", { link = "Comment" })
hi("@string", { link = "String" })
hi("@string.regex", { fg = p.cyan })
hi("@number", { link = "Number" })
hi("@boolean", { link = "Boolean" })
hi("@constant", { link = "Constant" })
hi("@constant.builtin", { fg = p.blue })
hi("@keyword", { link = "Keyword" })
hi("@keyword.function", { fg = p.pink, bold = true })
hi("@function", { link = "Function" })
hi("@function.builtin", { fg = p.green, bold = true })
hi("@constructor", { fg = p.white })
hi("@type", { link = "Type" })
hi("@type.builtin", { fg = p.white })
hi("@variable", { fg = p.peach })
hi("@variable.builtin", { fg = p.green })
hi("@property", { fg = p.fg })
hi("@field", { fg = p.fg })
hi("@parameter", { fg = p.white })
hi("@punctuation", { fg = p.fg_dim })
hi("@punctuation.delimiter", { fg = p.fg_dim })
hi("@tag", { fg = p.orange })
hi("@tag.attribute", { fg = p.peach })
hi("@tag.delimiter", { fg = p.fg_dim })

hi("LazyNormal", { fg = p.fg, bg = p.bg })
hi("SnacksDashboardHeader", { fg = p.pink, bold = true })
hi("SnacksDashboardTitle", { fg = p.pink, bold = true })
hi("SnacksDashboardKey", { fg = p.white })
hi("SnacksDashboardDesc", { fg = p.white })
hi("SnacksDashboardIcon", { fg = p.white })
hi("SnacksDashboardSpecial", { fg = p.white })
hi("MiniStarterHeader", { fg = p.pink, bold = true })
hi("MiniStarterItem", { fg = p.white })
hi("MiniStarterItemPrefix", { fg = p.white })
hi("MiniStarterSection", { fg = p.white })
hi("MasonNormal", { fg = p.fg, bg = p.bg_dark })

hi("TelescopeNormal", { fg = p.fg, bg = p.bg_dark })
hi("TelescopeBorder", { fg = p.border, bg = p.bg_dark })
hi("TelescopeTitle", { fg = p.yellow, bg = p.bg_dark, bold = true })
hi("TelescopePromptNormal", { fg = p.white, bg = p.bg_line })
hi("TelescopePromptBorder", { fg = p.pink, bg = p.bg_line })
hi("TelescopePromptTitle", { fg = p.bg_dark, bg = p.pink, bold = true })
hi("TelescopePreviewTitle", { fg = p.bg_dark, bg = p.orange, bold = true })
hi("TelescopeResultsTitle", { fg = p.bg_dark, bg = p.green, bold = true })
hi("TelescopeSelection", { fg = p.bg_dark, bg = p.yellow, bold = true })
hi("TelescopeMatching", { fg = p.pink, bold = true })

hi("WhichKey", { fg = p.yellow })
hi("WhichKeyGroup", { fg = p.orange })
hi("WhichKeyDesc", { fg = p.fg_soft })
hi("WhichKeySeperator", { fg = p.border })
hi("WhichKeySeparator", { fg = p.border })
hi("WhichKeyFloat", { bg = p.bg_dark })
hi("WhichKeyBorder", { fg = p.border, bg = p.bg_dark })

hi("NeoTreeNormal", { fg = p.fg, bg = p.bg_dark })
hi("NeoTreeNormalNC", { fg = p.fg, bg = p.bg_dark })
hi("NeoTreeFloatBorder", { fg = p.border, bg = p.bg_dark })
hi("NeoTreeFloatTitle", { fg = p.yellow, bg = p.bg_dark, bold = true })
hi("NeoTreeTitleBar", { fg = p.bg_dark, bg = p.pink, bold = true })
hi("NeoTreeDirectoryName", { fg = p.blue_bright })
hi("NeoTreeDirectoryIcon", { fg = p.orange })
hi("NeoTreeFileName", { fg = p.fg })
hi("NeoTreeFileNameOpened", { fg = p.yellow })
hi("NeoTreeRootName", { fg = p.green, bold = true })
hi("NeoTreeIndentMarker", { fg = p.border })
hi("NeoTreeGitAdded", { fg = p.green })
hi("NeoTreeGitModified", { fg = p.yellow })
hi("NeoTreeGitDeleted", { fg = p.red })

hi("SnacksIndent", { fg = p.border })
hi("SnacksIndentScope", { fg = p.pink })

hi("CmpItemAbbr", { fg = p.fg_soft })
hi("CmpItemAbbrMatch", { fg = p.pink, bold = true })
hi("CmpItemAbbrMatchFuzzy", { fg = p.orange, bold = true })
hi("CmpItemMenu", { fg = p.comment })
hi("CmpItemKind", { fg = p.orange })
hi("CmpItemKindFunction", { fg = p.fg })
hi("CmpItemKindMethod", { fg = p.fg })
hi("CmpItemKindVariable", { fg = p.peach })
hi("CmpItemKindKeyword", { fg = p.pink })
hi("CmpItemKindClass", { fg = p.white })
hi("CmpItemKindModule", { fg = p.cyan })

hi("GitSignsAdd", { fg = p.green })
hi("GitSignsChange", { fg = p.yellow })
hi("GitSignsDelete", { fg = p.red })
