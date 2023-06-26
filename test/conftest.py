import pytest

def pytest_addoption(parser):
    #Update reference test results for selected configurations. Possible values:
    #                                                                       'log': Update tracing logs
    #                                                                       'out': Update reference results
    parser.addoption('--update', default='FALSE', help='Update the reference output with the results of this test run.')
    parser.addoption('--margin', default='1', help='Set maximum deviation of test results from reference.')

@pytest.fixture
def update(request):
    """Store current results as new reference results."""
    return request.config.getoption("--update")

@pytest.fixture
def margin(request):
    """Set sensitivity level as percentage of max. deviation from reference."""
    return request.config.getoption("--margin")
