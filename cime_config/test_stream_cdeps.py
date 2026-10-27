"""
Test %daterange in stream_cdeps.py.

usage: CIMEROOT=/path/to/cime python test_stream_cdeps.py
"""
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


if __name__ == "__main__":
    test_daterange()
    print("PASS")
