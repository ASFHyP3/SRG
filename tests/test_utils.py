import os
import subprocess
from pathlib import Path
from unittest import mock

import pytest

from srg import utils


def test_create_param_file(tmp_path):
    dem_path = tmp_path / 'elevation.dem'
    dem_rsc_path = tmp_path / 'elevation.dem.rsc'
    output_dir = tmp_path
    utils.create_param_file(dem_path, dem_rsc_path, output_dir)
    assert (tmp_path / 'params').exists()

    with open(tmp_path / 'params') as f:
        lines = [x.strip() for x in f.readlines()]

    assert len(lines) == 2
    assert lines[0] == str(dem_path)
    assert lines[1] == str(dem_rsc_path)


def test_get_proc_home(tmp_path, monkeypatch):
    with monkeypatch.context() as m:
        m.setenv('PROC_HOME', str(tmp_path))
        assert utils.get_proc_home() == tmp_path

    with monkeypatch.context() as m:
        m.delenv('PROC_HOME', raising=False)
        msg = 'PROC_HOME environment variable is not set.*'
        with pytest.raises(ValueError, match=msg):
            utils.get_proc_home()


def test_get_netrc(monkeypatch):
    with monkeypatch.context() as m:
        m.setattr(utils, 'system', lambda: 'Windows')
        assert utils.get_netrc() == Path.home() / '_netrc'

    with monkeypatch.context() as m:
        m.setattr(utils, 'system', lambda: 'Linux')
        assert utils.get_netrc() == Path.home() / '.netrc'


def test_set_creds(monkeypatch):
    with monkeypatch.context() as m:
        m.delenv('TEST_USERNAME', raising=False)
        m.delenv('TEST_PASSWORD', raising=False)
        utils.set_creds('test', 'foo', 'bar')
        assert os.environ['TEST_USERNAME'] == 'foo'
        assert os.environ['TEST_PASSWORD'] == 'bar'


def test_find_creds_in_env(monkeypatch):
    with monkeypatch.context() as m:
        m.setenv('TEST_USERNAME', 'foo')
        m.setenv('TEST_PASSWORD', 'bar')
        assert utils.find_creds_in_env('TEST_USERNAME', 'TEST_PASSWORD') == (
            'foo',
            'bar',
        )

    with monkeypatch.context() as m:
        m.delenv('TEST_USERNAME', raising=False)
        m.delenv('TEST_PASSWORD', raising=False)
        assert utils.find_creds_in_env('TEST_USERNAME', 'TEST_PASSWORD') == (None, None)


def test_find_creds_in_netrc(tmp_path, monkeypatch):
    with monkeypatch.context() as m:
        m.setattr(utils, 'get_netrc', lambda: tmp_path / '.netrc')
        (tmp_path / '.netrc').write_text('machine test login foo password bar')
        assert utils.find_creds_in_netrc('test') == ('foo', 'bar')

    with monkeypatch.context() as m:
        m.setattr(utils, 'get_netrc', lambda: tmp_path / '.netrc')
        (tmp_path / '.netrc').write_text('')
        assert utils.find_creds_in_netrc('test') == (None, None)


def test_call_stanford_module(monkeypatch):
    with monkeypatch.context() as m:
        mock_run = mock.Mock()
        m.setattr(subprocess, 'run', mock_run)
        m.setenv('PROC_HOME', '.')
        utils.call_stanford_module('foo/bar.py', ['arg1', 'arg2'])
        mock_run.assert_called_once_with([Path('foo/bar.py'), 'arg1', 'arg2'], cwd=Path.cwd(), check=True)


def test_get_s3_args():
    s3_uri_1 = 's3://foo/bar.zip'
    s3_uri_2 = 's3://foo/bing/bong/bar.zip'
    dest_dir = Path('output')
    assert utils.get_s3_args(s3_uri_1) == ('foo', 'bar.zip', Path.cwd() / 'bar.zip')
    assert utils.get_s3_args(s3_uri_2, dest_dir) == (
        'foo',
        'bing/bong/bar.zip',
        dest_dir / 'bar.zip',
    )
