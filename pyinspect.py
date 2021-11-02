# SPDX-License-Identifier: GPL-3.0-or-later

import json
from inspect import getmembers, isbuiltin, ismethod
from itertools import filterfalse
from typing import Dict

_pyinspect_ITEM_CAP = 5
_pyinspect_STR_CAP = 80


def _pyinspect_inspect_object(obj):
    """Creates a dict of relevant fields in obj and their values"""

    def underline_count(member):
        key, val = member
        type_weight = 3 if ismethod(val) else 0
        return key.count("_", 0, 2) * 2 + type_weight

    def stringify_val(member):
        key, val = member
        if type(val) is str:
            return key, '"{}"'.format(val)
        else:
            try:
                return key, json.dumps(val, indent=4)
            except TypeError:
                return key, f"{str(val)} {str(type(val))}"

    def is_trash(member):
        key, val = member
        return (
            key in ["__doc__", "__class__", "__hash__", "__dict__"]
            or ismethod(val)
            or isbuiltin(val)
            or type(val).__name__ == "method-wrapper"
        )

    # getmembers() returns a tuple of (fieldname, value)
    members = sorted(getmembers(obj), key=underline_count)
    members = filterfalse(is_trash, members)
    members = map(stringify_val, members)

    return dict(members)


def _pyinspect_add_quotes(key):
    """
    Surrounds string key with extra quotes because Emacs parses them as just symbols
    and makes it hard to distinguish between them and non-string symbols
    """
    return '"{}"'.format(key) if type(key) is str else key


def _pyinspect_trim(obj, elem_cap, str_cap):
    """
    If obj is a sequence (dict/list/tuple), takes its first elem_cap elements and drops the rest.
    Also adds a cute ellipsis before the closing bracket to signal that it has been trimmed.
    Returns a pretty-printed string of the sequence, formatted by json.dumps with indent=4.

    If it's a string or any other kind of object, coerce it into a string and take the first
    str_cap characters. AND add a cute ellipsis.

    >>> _pyinspect_trim("abcde", elem_cap=3, str_cap=3)
    'abc...'

    >>> print(_pyinspect_trim([1, 2, 3, 4], elem_cap=3, str_cap=3))
    [
        1,
        2,
        3
        ...
    ]

    >>> print(_pyinspect_trim({x: x + 1 for x in range(10)}, elem_cap=3, str_cap=3))
    {
        "0": 1,
        "1": 2,
        "2": 3
        ...
    }
    """

    def trim_seq(seq):
        if type(seq) is dict:
            return _pyinspect_take_dict(seq, elem_cap)
        elif type(seq) in (tuple, list):
            return seq[:elem_cap]

    if type(obj) in (dict, tuple, list) and len(obj) > elem_cap:
        jsondump = json.dumps(trim_seq(obj), indent=4)
        return f"{jsondump[:-1]}    ...\n{jsondump[-1]}"

    s = str(obj)
    return f"{s[:str_cap]}..." if len(s) > str_cap else s


def _pyinspect_take_dict(d: Dict, n: int):
    "Returns a new dictionary with the first n pairs from d"

    def iterator():
        i = 0
        for item in d.items():
            if i == n:
                break
            yield item
            i += 1

    return dict(iterator())


def _pyinspect(obj):
    "Dispatches the appropriate inspection according to obj type"
    if type(obj) in (str, bool, int, float, complex):
        return {"type": "primitive", "value": obj}

    elif type(obj) in (tuple, list):
        return {
            "type": "collection",
            "value": [
                _pyinspect_trim(item, _pyinspect_ITEM_CAP, _pyinspect_STR_CAP)
                for item in obj
            ],
        }

    elif type(obj) is dict:
        return {
            "type": "dict",
            "value": {
                _pyinspect_add_quotes(k): _pyinspect_trim(
                    v, _pyinspect_ITEM_CAP, _pyinspect_STR_CAP
                )
                for (k, v) in obj.items()
            },
        }

    else:
        return {"type": "object", "value": _pyinspect_inspect_object(obj)}


def _pyinspect_json(obj):
    def default_func(problematic_obj):
        return _pyinspect(problematic_obj)["value"]

    print(json.dumps(_pyinspect(obj), indent=4, default=default_func))
