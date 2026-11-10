from pathlib import Path
from unittest import mock

import pytest
from shapely.geometry import box

from srg import dem, utils


def test_download_dem_for_srg(monkeypatch):
    with monkeypatch.context() as m:
        mock_ensure_egm_model_available = mock.Mock()
        m.setattr(dem, 'ensure_egm_model_available', mock_ensure_egm_model_available)
        mock_call_stanford_module = mock.Mock()
        m.setattr(utils, 'call_stanford_module', mock_call_stanford_module)
        dem.download_dem_for_srg(list(box(0, 1, 2, 3).bounds), Path.cwd())
        mock_ensure_egm_model_available.assert_called_once()
        mock_call_stanford_module.assert_called_once_with(
            'DEM/createDEMcop.py',
            [
                str(Path.cwd() / 'elevation.dem'),
                str(Path.cwd() / 'elevation.dem.rsc'),
                3,
                1,
                0,
                2,
                '6',
                '2',
            ],
            work_dir=Path.cwd(),
        )
        bad_bboxs = [
            [0.0, 1.0, -1.0, 1.0],
            [1.0, 1.0, -1.0, 2.0],
            [1.0, 0.0, 2.0, -1.0],
        ]
        for bbox in bad_bboxs:
            with pytest.raises(ValueError, match=r'Improper bounding box formatting*'):
                dem.download_dem_for_srg(bbox, Path.cwd())
