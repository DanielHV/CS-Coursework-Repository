# Computación distribuida
## Práctica 5

### Broadcast

**Nota**: Para simular `wait(1-5)` se hace uso de `yield env.timeout(randint(1,5))`, lo cual "pausa" la ejecución entre 1 y 5 unidades de tiempo, dependiendo del número aleatorio.

El nodo inicial recibe la entrada de la función y asigna `self.mensaje`, luego envía los datos a cada vecino, registrando unu evento de enviado para cada uno y aumentando en 1 su reloj.

Para el resto de nodos se inicia con un timeout, una vez que termina el timeout se recibe el mensaje del canal de entrada.

Se asigna `self.mensaje` con los datos recibidos y el reloj se actualiza por aquel que vaya más adelantado (tenga un número mayor) y avanza en 1.

Se registra un evento de recibido y después de un nuevo timeout se envían los datos recibidos a todos los vecinos, registrando un evento de enviado para cada uno y aumentando en 1 su reloj.

### DFS

**Nota**: Para simular `wait for a random delay` se hace uso nuevamente de `yield env.timeout(randint(1,5))`

**Nota**: Al almacenar los eventos, se obtiene un `TypeError` en las pruebas pues no se puede usar un set (el conjunto de visitados) para crear una llave, por lo que se convierte a tupla para que funcione correctamente.

El nodo inicial selecciona a su primer vecino como hijo y le envía un mensaje "GO" con un set que solo incluye al nodo inicial, registrando un evento de enviado y aumentando en 1 su reloj.

Para el resto de nodos se inicia con un timeout, una vez que termina el timeout se recibe el mensaje del canal de entrada.

El reloj se actualiza por aquel que vaya más adelantado (tenga un número mayor en cualquier posición del arreglo) y avanza en 1.

Se registra un evento de recibido, dependiendo del tipo de mensaje recibido sucede lo siguiente:

- Si es un mensaje de tipo "GO": se asigna el nodo que envía el mensaje como padre; si aún hay vecinos no visitados se elije el primero y se envía un mensaje "GO" con los nodos visitados hasta ahora incluyendose, registrando un evento de enviado y aumentando en 1 su reloj, si todos los vecinos han sido visitados se envía un mensaje "BACK" al padre con todos los nodos visitados hasta ahora incluyendose, registrando un evento de enviado y aumentando en 1 su reloj.

- Si es un mensaje de tipo "BACK": si aún hay vecinos no visitados se elije el primero y se envía un mensaje "GO" con los nodos visitados hasta ahora, registrando un evento de enviado y aumentando en 1 su reloj, si todos los vecinos han sido visitados se envía un mensaje "BACK" al padre con todos los nodos visitados hasta ahora, registrando un evento de enviado y aumentando en 1 su reloj. Finalmente, si todos los vecinos han sido visitados y el nodo actual es su propio padre (es la raíz), se termina el algoritmo.