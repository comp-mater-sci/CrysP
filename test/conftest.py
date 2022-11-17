def pytest_addoption(parser):
    #Update reference test results for selected configurations. Possible values:
    #                                                                       'log': Update tracing logs
    #                                                                       'out': Update reference results
    parser.addoption('--update', default='FALSE', help='Update the reference output with the results of this test run.')
