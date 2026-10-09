def test_hyp3_srg(script_runner):
    ret = script_runner.run(['python', '-m', 'srg', '-h'])
    assert ret.success
