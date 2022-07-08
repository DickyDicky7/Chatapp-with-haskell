const submitUsername = document.querySelector('#submit-username');
const usernameInput = document.querySelector('#username');
let username = null;
submitUsername.addEventListener('click', () => {
  username = usernameInput.value;
});

function connect() {
  const ws = new WebSocket('ws://localhost:8080');
  ws.onopen = function () {
    const send = document.querySelector('#send');
    const input = document.querySelector('#input');
    send.addEventListener('click', () => {
      ws.send(
        JSON.stringify({
          name: username,
          message: input.value,
        })
      );
      setTimeout(function () {
        input.value = '';
      }, 1000);
    });
  };

  ws.onmessage = function (event) {
    const { message, time, name } = JSON.parse(event.data);
    const content = document.querySelector('#content');
    const chatMessage = document.createElement('p');
    chatMessage.textContent = `${time}, ${name}: ${message}`;
    content.append(chatMessage);
  };

  ws.onclose = function () {
    setTimeout(function () {
      connect();
    }, 1000);
  };

  ws.onerror = function () {
    ws.close();
  };
}

connect();
