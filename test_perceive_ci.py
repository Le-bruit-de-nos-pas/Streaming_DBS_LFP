from perceive_ci import perceive_ci

def test_perceive_ci():
    # Test Scenario 1: Single channel and a list of possible channels
    channels_1 = "channel1"
    possible_list_1 = ["channel1", "channel2", "channel3"]
    expected_index_1 = [0]
    expected_names_1 = ["channel1"]

    result_1 = perceive_ci(channels_1, possible_list_1)
    assert result_1 == (expected_index_1, expected_names_1), "Test Scenario 1 failed"

    # Test Scenario 2: Single channel not in possible list
    channels_2 = "channel4"
    possible_list_2 = ["channel1", "channel2", "channel3"]
    expected_index_2 = []
    expected_names_2 = []

    result_2 = perceive_ci(channels_2, possible_list_2)
    assert result_2 == (expected_index_2, expected_names_2), "Test Scenario 2 failed"

    # Test Scenario 3: Multiple channels and possible lists with partial match
    channels_3 = ["chann", "chan"]
    possible_list_3 = ["channel1", "channel2", "channel3"]
    expected_index_3 = [[0, 1, 2], [0, 1, 2]]
    expected_names_3 = [['channel1', 'channel2', 'channel3'], ['channel1', 'channel2', 'channel3']]

    result_3 = perceive_ci(channels_3, possible_list_3)
    assert result_3 == (expected_index_3, expected_names_3), "Test Scenario 3 failed"

    print("All test cases passed!")

# Run the test
test_perceive_ci()

