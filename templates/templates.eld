fundamental-mode ;; Available everywhere
;; use 'a' to prefix so some template can work

(today (format-time-string "%Y-%0m-%0d"))
(afile buffer-file-name)
(afilename (file-name-nondirectory buffer-file-name))
(filename-title (upcase-initials (replace-regexp-in-string
                                     "[-_]"
                                     " "
                                     (file-name-base buffer-file-name))))
(info "Author: " user-full-name "\n"
 "Time: " (current-time-string) "\n" q)

(el "El Psy Congroo" q)

org-mode

(inline "src_" p "[:exports code]{" r> "}")

rust-ts-mode

(pt "fn pt<T>(_: &T) { println!(\"{}\", std::any::type_name::<T>()) }" q)
(dead "#[allow(dead_code)] // FIXME" q)

typst-ts-mode

(font "#set text(font: \"LXGW WenKai\", size: 13pt) " q)

markdown-mode

(meta "+++
title = \"" (p (upcase-initials (replace-regexp-in-string (rx (or "-" "_")) " " (file-name-base buffer-file-name)))) """\"
description = \"" p "\"
date = " (format-time-string "%Y-%0m-%0d") "
updated = " (format-time-string "%Y-%0m-%0d") "
draft = false

[taxonomies]
tags = ["(p "\"Emacs\"" )"]

[extra]
math = false
math_auto_render = false
keywords = \"" (p "A, B") "\"
toc = true
series = \"Features\" # TODO
+++
" q)

(more "<!-- more -->" q)

java-ts-mode
(pl "System.out.println(String.format(\"%s\", " q "));")

zig-ts-mode

(pl "std.debug.print(\"{" (p "") "}\\n\", .{" q "});")
(pla "std.debug.print(\"{s}\\n\", .{std.json.fmt(" q ", .{})});")
(pt "std.debug.print(\"{}\", .{@TypeOf(" q ")});")
(test_eq "try std.testing.expectEqual(" q ")")
(test_eq_str "try std.testing.expectEqualStrings(" q ")")
(gpa "var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
defer {
    const deinit_status = gpa.deinit();
    if (deinit_status == .leak) @panic(\"Memory Leak!\");
}")

js-ts-mode

(pl "console.log(" q ")")

web-mode

(style "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (p "styles.css") "\" />" q)
(script "<script src=\"" (p "main.js") "\"></script>" q)
(script-module "<script type=\"module\" src=\"" (p "main.js") "\"></script>" q)
