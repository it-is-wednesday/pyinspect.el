# SPDX-License-Identifier: GPL-3.0-or-later

import json
from inspect import getmembers, isbuiltin, ismethod
from itertools import filterfalse


def _pyinspect_inspect_object(obj):
    def underline_count(member):
        key, val = member
        type_weight = 3 if ismethod(val) else 0
        return key.count("_", 0, 2) * 2 + type_weight

    def stringify_val(member):
        key, val = member
        return key, '"{}"'.format(val) if type(val) is str else str(val)

    def is_trash(member):
        key, val = member
        return (
            key in ["__doc__", "__class__", "__hash__"]
            or ismethod(val)
            or isbuiltin(val)
            or type(val).__name__ == "method-wrapper"
        )

    # getmembers() returns a tuple of (fieldname, value)
    members = sorted(getmembers(obj), key=underline_count)
    members = filterfalse(is_trash, members)
    members = map(stringify_val, members)

    return dict(members)


def _pyinspect(obj):
    if type(obj) in (str, bool, int, float, complex):
        return {"type": "primitive", "value": obj}

    elif type(obj) in (tuple, list):
        return {"type": "collection", "value": obj}

    elif type(obj) is dict:

        def add_quotes(key):
            """
            Surrounds string key with extra quotes because Emacs parses them as just symbols
            and makes it hard to distinguish between them and non-string symbols
            """
            return '"{}"'.format(key) if type(key) is str else key

        return {"type": "dict", "value": {add_quotes(k): v for (k, v) in obj.items()}}

    else:
        return {"type": "object", "value": _pyinspect_inspect_object(obj)}


def _pyinspect_json(obj):
    def default_func(problematic_obj):
        return _pyinspect(problematic_obj)["value"]

    print(json.dumps(_pyinspect(obj), indent=4, default=default_func))
