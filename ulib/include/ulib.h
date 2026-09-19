#pragma once

// Display name used in the login greeting and the MSSP server-status reply.
#define MUD_NAME "ulib"

// In-game object path cloned by connect(), rooted at LPC_LIB_DIR rather than the host root.
#define PLAYER_OBJECT "/obj/player"

// In-game room path preloaded at boot and entered after a guest chooses a name.
#define START_ROOM "/room/lounge"

// Inclusive name-length limits; validation also requires ASCII letters and reserves "quit".
#define MIN_NAME_LENGTH 3
#define MAX_NAME_LENGTH 16

// Maximum number of Unicode characters in say/emote text, excluding the verb and name prefix.
#define MAX_CHAT_LENGTH 400
