import numpy as np

from srg import gnss


HOLDINGS = """Sta  Lat(deg)   Long(deg) Hgt(m)  X(m) Y(m) Z(m) Dtbeg Dtend Dtmod NumSol
KULE  19.2486  -155.3233   58.160 -5479410.0 -2509566.0 2088690.0 1996-08-31 2026-11-01 2026-11-02   9000
OLDS  19.3000  -155.2000   10.000 -5479410.0 -2509566.0 2088690.0 2001-01-01 2010-01-01 2010-01-02   3000
FARX  21.3000   202.1000   10.000 -5479410.0 -2509566.0 2088690.0 2001-01-01 2026-01-01 2026-01-02   3000
"""

TENV3 = """site YYMMMDD yyyy.yyyy __MJD week d reflon _e0(m) __east(m) ____n0(m) _north(m) u0(m) ____up(m) _ant(m)
KULE 23JAN12 2023.0329 59956 2244 4 -155.3  -2447 -1.100000   2129186  0.600000    58  0.100000  0.0000
KULE 23JAN13 2023.0356 59957 2244 5 -155.3  -2447 -1.101000   2129186  0.601000    58  0.102000  0.0000
"""


def test_find_sites():
    snwe = (19.2, 19.5, -155.4, -155.0)
    assert gnss.find_sites(HOLDINGS, snwe, '2023-01-12', '2025-12-27') == [('KULE', 19.2486, -155.3233)]


def test_read_tenv3():
    dates, enu = gnss.read_tenv3(TENV3)
    assert dates.tolist() == [np.datetime64('2023-01-12'), np.datetime64('2023-01-13')]
    np.testing.assert_allclose(enu[1], [-1.101, 0.601, 0.102])
