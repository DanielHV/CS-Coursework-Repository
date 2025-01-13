# Computación distribuida
## Práctica 3

### BFS

El nodo se pone a la espera de mensajes, al recibir un mensaje se pueden dar los siguientes casos.

- Si el nodo es el nodo distinguido (id_nodo == 0), envía un mensaje "GO" a todos sus vecinos.
- Cuando un nodo recibe un mensaje "GO" y no tiene padre, se asigna el nodo que envió el mensaje como su padre y reenvía el mensaje "GO" a sus vecinos (excepto al padre).
- Cuando un nodo recibe un mensaje "GO" pero ya tiene un padre asignado, envía de vuelta un mensaje "BACK" con respuesta "no" al nodo que envió "GO".
- Cuando un nodo ha recibido todos los mensajes esperados, envía un mensaje "BACK" con respuesta "si" a su padre.
- Cuando un nodo recibe un mensaje "BACK" con respuesta "si", decrece su contador de mensajes esperados, y asigna al nodo que envió "BACK" como hijo.
- Cuando un nodo recibe un mensaje "BACK" con respuesta "no", decrece su contador pero no asigna ningún hijo.
- Si el nodo distinguido recibe un mensaje "BACK", no hay más pasos por realizar.

Nota: En el caso donde el nodo distinguido selecciona los vecinos a los que se va a enviar "GO", se obtiene un ValueError al intentar extraer de la lista de vecinos al nodo mismo, pues no es un vecino de si mismo, esto se soluciona manejando el error con un bloque try except.

### DFS

- Si el nodo es el nodo distinguido (id_nodo == 0), envía un mensaje "GO" al vecino de menor id (siendo el nodo distinguido el único visitado) y lo asigna como hijo.
- Cuando un nodo recibe un mensaje "GO", si aún hay vecinos no visitados, se asigna el nodo que envió el mensaje como su padre y reenvía el mensaje "GO" al vecino no visitado de menor id y lo asigna como hijo, actualizando los nodos visitados.
- Cuando un nodo recibe un mensaje "GO", Si todos los vecinos fueron visitados, envía un mensaje "BACK" a su padre actualizando los nodos visitados.
- Cuando un nodo recibe un mensaje "BACK", si aún hay vecinos no visitados, envía "GO" al vecino de menor id sin actualizar los nodos visitados.
- Cuando un nodo recibe un mensaje "BACK", si todos los vecinos fueron visitados, envía un mensaje "BACK" a su padre sin actualizar los nodos visitados.
- Si el nodo distinguido recibe un mensaje "BACK", no hay más pasos por realizar.
