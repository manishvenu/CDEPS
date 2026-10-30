"""
Test %daterange in stream_cdeps.py.

usage: CIMEROOT=/path/to/cime python test_stream_cdeps.py
"""
import logging
import os
import sys
import tempfile

_CIMEROOT = os.environ.get("CIMEROOT")
if _CIMEROOT is None:
    raise SystemExit("ERROR: must set CIMEROOT environment variable")
sys.path.append(_CIMEROOT)
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from stream_cdeps import StreamCDEPS


def test_daterange():
    """%daterange becomes the files that cover the stream years, in date order."""
    with tempfile.TemporaryDirectory() as directory:
        for name in [
            "case.cam.h2.UBOT.1999010100-2000010100.nc",  # ends Jan 1 2000, so 1999 only
            "case.cam.h2.UBOT.2001010100-2002010100.nc",
            "case.cam.h2.UBOT.2000010100-2001010100.nc",
            "case.cam.h2.UBOT.2002010100-2003010100.nc",
            "case.cam.h2.U10.2000010100-2001010100.nc",  # another variable
        ]:
            open(os.path.join(directory, name), "w").close()

        files = StreamCDEPS._sub_daterange(
            "UBOT", os.path.join(directory, "case.cam.h2.UBOT.%daterange.nc"), 2000, 2001
        )

        assert files.split("\n") == [
            os.path.join(directory, "case.cam.h2.UBOT.2000010100-2001010100.nc"),
            os.path.join(directory, "case.cam.h2.UBOT.2001010100-2002010100.nc"),
        ], files


def test_check_daterange_start():
    """The run must start inside a %daterange stream's years; starting after
    year_first gives a warning."""
    check = StreamCDEPS._check_daterange_start
    warnings = []
    handler = logging.Handler()
    handler.emit = warnings.append
    logging.getLogger("stream_cdeps").addHandler(handler)

    check("UBOT", 2000, "extend", 2000, 2001, 2000)  # starts in year_first
    check("UBOT", 1850, "extend", 2000, 2001, 1850)  # model 1850 is data 2000
    check("UBOT", 1850, "cycle", 2000, 2001, 2000)  # cycle wraps every year in
    assert not warnings, [w.getMessage() for w in warnings]

    check("UBOT", 2000, "extend", 1920, 2005, 1920)  # 80 years of files to skip
    assert "after year_first 1920" in warnings[-1].getMessage(), warnings[-1].getMessage()

    try:
        check("UBOT", 1999, "extend", 2000, 2001, 2000)
    except SystemExit as error:  # CIME's expect raises CIMEError, a SystemExit
        assert "outside its years 2000 to 2001" in str(error), error
    else:
        raise AssertionError("a run starting before year_first should stop")


if __name__ == "__main__":
    test_daterange()
    test_check_daterange_start()
    print("PASS")
