fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

mhtml-mode

(p "<" (s tag) ">" r> "</" (s tag) ">") ;; p stands for '<', cuttently it doesn't support special character

org-mode

(inline "src_" p "[:exports code]{" r> "}")

java-ts-mode

(pkg "package " q ";")
(imp "import " q ";")
(pl "System.out.println(\"" q "\");")
(class "public class " (p (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) " {" n> q n "}")
(main "public static void main(String[] args) {" n> q n> "}")
(if "if (" p ") {" n> q n> "}")
(elif "else if (" p ") {" n> q n> "}")
(else "else {" n> q n> "}")
(for "for (" p ") {" n> q n> "}")

rust-ts-mode

(pt "fn pt<T>(_: &T) { println!(\"{}\", std::any::type_name::<T>()) }" q)

rust-mode

(pt "fn pt<T>(_: &T) { println!(\"{}\", std::any::type_name::<T>()) }" q)

typst-ts-mode

(font "#set text(font: \"LXGW WenKai\", size: 13pt) " q)
