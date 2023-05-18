cd "$(dirname "$(realpath "$0")")"
if test "$(git status -s | wc -l)" -gt 0; then
    _DIRTY="+""$(date -Is | sed 's|[+].*||;s|[-T:]||g')"
else
    _DIRTY=""
fi

echo "$(git describe --tags | sed 's|-g.*||g;')"$_DIRTY
