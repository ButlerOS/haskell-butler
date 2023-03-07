// This is the electron preload script, to connect the main and browser context.

const { contextBridge, ipcRenderer } = require('electron')

const glRequests = {}

contextBridge.exposeInMainWorld('butlerElectron', {
  // Check if ssh-agent is available.
  hasSshAgent: () =>  ipcRenderer.invoke('hasSshAgent'),

  // Send data to an ssh-agent request.
  sendSshAgent: (requestID, data) =>  {
    console.log("Sending data to main", requestID, data)
    glRequests[requestID].postMessage(data)
  },

  // Initialize a new sshh-agent request.
  connectSshAgent: (requestID, onData) =>  {
      if (glRequests[requestID]) {
          console.error("Request already running...", requestID)
          return
      }
      console.log("New ssh-agent request", requestID)

      // Create communication channel between main and render.
      const channel = new MessageChannel()
      ipcRenderer.postMessage('connectSshAgent', null, [channel.port1])

      // Setup the render listener.
      channel.port2.onmessage = (ev) =>  {
        console.log("Sending data back to browser", ev)
        onData(ev.data)
      }

      // Store the render side of the channel. (Unfortunately this can't be returned to the browser).
      glRequests[requestID] = channel.port2
  }
})
