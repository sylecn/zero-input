#!/usr/bin/env python3
# coding=utf-8

"""
build zero-input.el from zero-input.el.in and other el files
"""

import re
import logging

logger = logging.getLogger(__name__)


def placeholder_for_file(fn):
    """return placeholder that should be used for filename.

    """
    return "INCLUDE_" + fn.replace('-', '_').replace('.', '_').upper()


def test_placeholder_for_file():
    assert placeholder_for_file("zero-input-panel.el") == (
        "INCLUDE_ZERO_INPUT_PANEL_EL")
    assert placeholder_for_file("zero-input-pinyin-service.el") == (
        "INCLUDE_ZERO_INPUT_PINYIN_SERVICE_EL")


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


def get_zero_input_version():
    """get zero-input-version from zero-input-framework.el file.

    """
    with open("zero-input-framework.el", encoding='utf-8') as f:
        for line in f:
            mo = re.match('^\\(setq zero-input-version "(.*)"', line)
            if mo:
                return mo.group(1)
    raise RuntimeError("get zero-input-version failed.")


def main():
    logging.basicConfig(
        format='%(asctime)s [%(module)s] %(levelname)-8s %(message)s',
        level=logging.INFO)
    zero_input_version = get_zero_input_version()
    with open("zero-input.el.in") as tpl:
        content = tpl.read().replace('PKG_VERSION', zero_input_version)
    expanded_content = expand_placeholder_for_files(content, [
        "zero-input-panel.el",
        "zero-input-framework.el",
        "zero-input-table.el",
        "zero-input-pinyin-service.el",
        "zero-input-pinyin.el",
        ])
    with open('zero-input.el', 'r', encoding="utf-8") as fin:
        old_content = fin.read()
    if expanded_content != old_content:
        with open('zero-input.el', 'w', encoding="utf-8") as out:
            out.write(expanded_content)


if __name__ == '__main__':
    main()
