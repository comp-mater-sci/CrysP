def pytest_addoption(parser):
    parser.addoption('--update', default='FALSE', help='Update the reference output with the results of this test run.')
