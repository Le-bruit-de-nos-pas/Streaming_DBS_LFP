def perceive_ci(channels, possible_list, exact=False):
    # Initialize lists to store the indexes and names of matches
    channel_indexes = []
    channel_names = []

    # Iterate through each channel
    for channel in channels:
        indexes = []
        names = []
        # Iterate through each possible channel in the list
        for i, possible_channel in enumerate(possible_list):
            # Check for exact match
            if exact and channel.lower() == possible_channel.lower():
                indexes.append(i)
                names.append(possible_channel)
            # Check for partial match
            elif not exact and channel.lower() in possible_channel.lower():
                indexes.append(i)
                names.append(possible_channel)
        # Store the matches for this channel
        channel_indexes.append(indexes)
        channel_names.append(names)

    return channel_indexes, channel_names


perceive_ci(["chann", "chan"], ["channel1", "channel2", "channel3"])

perceive_ci(["channel1"], ["channel1", "channel2", "channel3"])

