# Computación distribuida
## Práctica 2

### Explicación de implementaciones

#### 1. Método envia

Dada la lista de vecinos proporcionada, para cada uno de los vecinos se localiza su respectivo canal de entrada en el atributo `self.canales` y se crea un evento para cada uno donde se envía el mensaje (para ello se utiliza el método `put()` de `simpy.Store`), estos se agregan una lista de eventos, con la cual se usa el método `self.env.all_of(eventos)` para asegurar que todos terminen.

#### 2. Algoritmo de árbol generador

1. Para inciar el algoritmo, el nodo distinguido envía un mensaje GO a todos sus vecinos.

2. Cada nodo esperará a recibir un mensaje en su respectivo canal de entrada, los mensajes posibles son: `("GO", id)`, `("BACK", id)` y `("BACK", None)`(se envía una tupla como mensaje para poder obtener el identificador del nodo que envía el mensaje).

3. En el caso de recibir un mensaje GO y no se tiene un padre asignado, se asigna como padre al nodo que envió el mensaje GO y se definen los mensajes esperados (que serían de cada uno de sus vecinos excepto de quien recibió el mensaje GO) y se envían los respectivos mensajes GO. Si se tiene que no hay vecinos a los que enviar GO, se envía BACK al nodo que envió GO.

4. En el caso de recibir un mensaje GO y que ya haya un padre asignado, se envía un mensaje BACK sin identificador.

5. En el caso de recibir un mensaje BACK, se reduce el contandor de mensajes esperados en uno, y si el mensaje BACK contiene identificador, se agrega a la lista de hijos el nodo de quien se recibió el mensaje BACK. Si ya se recibieron todos los mensajes esperados, se envía iun mensaje BACK con identificador al padre asinado.

#### 3. Algoritmo broadcast

1. Para iniciar el algoritmo, el nodo distinguido define el mensaje que va a enviar y lo envía a cada uno de sus vecinos, en este caso, en la lista de vecinos se incluyen solo los hijos en el árbol.

2. Cada nodo esperará a recibir un mensaje en su respectivo canal de entrada, una vez recibido el mensaje, se asigna al atributo `self.mensaje`y nuevamente se envía a cada uno de sus vecinos (sus hijos en este caso).