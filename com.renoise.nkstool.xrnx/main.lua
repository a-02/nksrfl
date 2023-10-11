
local OscMessage = renoise.Osc.Message
local OscBundle = renoise.Osc.Bundle

local server, socket_error = renoise.Socket.create_server(
  "localhost", 8088, renoise.Socket.PROTOCOL_UDP)

local client, socket_error = renoise.Socket.create_client(
  "localhost", 9099, renoise.Socket.PROTOCOL_UDP)

if (socket_error) then
  renoise.app():show_warning(("you fucked up: '%s'"):format(socket_error))
  return
end



renoise.tool():add_menu_entry {
  name = "Main Menu:File:FUCK IT DUDE LETS GO BOWLIN'",
  invoke = function() 
    server:run {
      socket_message = function(socket, data)
        local message_or_bundle, osc_error = renoise.Osc.from_binary_data(data)
        if (message_or_bundle) then
        print(("Got: '%s'"):format(tostring(message_or_bundle)))
        oscClient();
      end
    end
  }
  end
}

function oscClient()
  local currentBpm = renoise.song().transport.bpm
  local isPlaying = renoise.song().transport.playing
  local songPosition = renoise.song().transport.playback_pos
  local isLooping = renoise.song().transport.loop_pattern
  local isBlockLooping = renoise.song().transport.loop_block_enabled
  client:send(
    OscMessage("/nks/all", {
      {tag="f", value=currentBpm},
      {tag="s", value=tostring(isPlaying)},
      {tag="s", value=tostring(songPosition)},
      {tag="s", value=tostring(isLooping)},
      {tag="s", value=tostring(isBlockLooping)}
    })
  )
end
