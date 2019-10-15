#!/usr/bin/env python3
# coding=utf-8

"""
build zero.el from zero.el.in and other el files
"""

import logging

logger = logging.getLogger(__name__)


def placeholder_for_file(fn):
    """return placeholder that should be used for filename.

    """
    return "INCLUDE_" + fn.replace('-', '_').replace('.', '_').upper()


def test_placeholder_for_file():
    assert placeholder_for_file("zero-panel.el") == "INCLUDE_ZERO_PANEL_EL"
    assert placeholder_for_file("zero-pinyin-service.el") == (
        "INCLUDE_ZERO_PINYIN_SERVICE_EL")


def get_el_file_body(fn):
    rows = []
    append = False
    with open(fn) as f:
        for line in f:
            if append:
                if '(require ' in line:
                    logger.debug("skipping require call: %s", line)
                    continue
                rows.append(line)
                if line.startswith('(provide'):
                    break
            else:
                if line.startswith(";;; Code:"):
                    rows.append(";; body of %s\n" % (fn,))
                    append = True
    return "".join(rows)


def expand_placeholder_for_files(old_content, filenames):
    """replace INCLUDE_FOO_BAR_EL by body of foo-bar.el.

    """
    result = old_content
    for fn in filenames:
        result = result.replace(placeholder_for_file(fn),
                                get_el_file_body(fn))
    return result


def main():
    logging.basicConfig(
        format='%(asctime)s [%(module)s] %(levelname)-8s %(message)s',
        level=logging.INFO)
    with open("zero.el.in") as tpl:
        content = tpl.read()
    expanded_content = expand_placeholder_for_files(content, [
        "zero-panel.el",
        "zero-framework.el",
        "zero-table.el",
        "zero-pinyin-service.el",
        "zero-pinyin.el",
        ])
    with open('zero.el', 'w') as out:
        out.write(expanded_content)


if __name__ == '__main__':
    main()
