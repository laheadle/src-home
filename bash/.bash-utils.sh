

# broken for dired bufs. todo replace ~ with HOME
# better:
# 10:45:lyn_headleyC02SGFJ4G8WP:bsx-ebook-plus-feedback → AVLN-16518-report-feedback emacsclient -e "(progn (set-buffer (car (buffer-list))) (expand-file-name default-directory))"
# "/Users/lyn_headley/Workspace/bento/bento-ui/src/app/bsx-reports/components/bsx-ebook-plus-feedback/"
function epwd() {
    emacsclient -e "(progn (set-buffer (car (buffer-list))) (pwd))" | bb -I --stream '(as-> *input* $ (str/split $ #" ") (second $) (println $))'
}

alias cepwd='cd $(epwd);echo $(pwd)'
