import simpy
from Nodo import *
from Canales.CanalRecorridos import *

# La unidad de tiempo
TICK = 1


class NodoBFS(Nodo):
    ''' 
    Implementa la interfaz de Nodo para el algoritmo de Broadcast utilizando BFS.
    
    Atributos:
        id_nodo (int): Identificador único del nodo.
        vecinos (list): Lista de nodos vecinos.
        canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
        canal_salida (CanalRecorridos): Canal de salida para enviar mensajes.
        padre (int): Identificador del nodo padre en el árbol BFS.
        hijos (list): Lista de identificadores de los nodos hijos en el árbol BFS.
        distancia (int): Distancia desde el nodo raíz.
        mensajes_esperados (int): Cantidad de mensajes que se esperan recibir.
    '''

    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida):
        '''
        Inicializa el nodo.
        
        Args:
            id_nodo (int): Identificador único del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
            canal_salida (CanalRecorridos): Canal de salida para enviar mensajes.
        '''
        # Aquí va tu implementacion
        self.id_nodo = id_nodo
        self.vecinos = vecinos
        self.canal_entrada = canal_entrada
        self.canal_salida = canal_salida
        self.padre = id_nodo
        self.hijos = None
        self.distancia = None
        self.mensajes_esperados = len(vecinos)

    def bfs(self, env):
        '''
        Algoritmo BFS.
        
        Args:
            env (simpy.Environment): El entorno de simulación de SimPy.
        
        Comportamiento:
            - Si el nodo es el nodo distinguido (id_nodo == 0), envía un mensaje "GO" a todos sus vecinos.
            - Cuando un nodo recibe un mensaje "GO" y no tiene padre, se asigna el nodo que envió el mensaje como su padre y reenvía el mensaje "GO" a sus vecinos (excepto al padre).
            - Cuando un nodo recibe un mensaje "GO" pero ya tiene un padre asignado, envía de vuelta un mensaje "BACK" con respuesta "no" al nodo que envió "GO".
            - Cuando un nodo ha recibido todos los mensajes esperados, envía un mensaje "BACK" con respuesta "si" a su padre.
            - Cuando un nodo recibe un mensaje "BACK" con respuesta "si", decrece su contador de mensajes esperados, y asigna al nodo que envió "BACK" como hijo.
            - Cuando un nodo recibe un mensaje "BACK" con respuesta "no", decrece su contador pero no asigna ningún hijo.
            - Si el nodo distinguido recibe un mensaje "BACK", no hay más pasos por realizar.
        '''
        yield env.timeout(TICK)

        # Tu implementacion va aqui abajo
        
        # ----- estructura de mensajes: (id, GO, distancia), (id, BACK, distancia, respuesta) -----
        
        # start
        if self.id_nodo == 0:
            self.canal_salida.envia((self.id_nodo, "GO", -1), [self.id_nodo])
            
        while True:
            mensaje = yield self.canal_entrada.get()
            
            if mensaje[1] == "GO":
                if self.padre == self.id_nodo:
                    self.padre = mensaje[0]; self.hijos = list(); self.distancia = mensaje[2] + 1
                    self.mensajes_esperados = len(self.vecinos) - 1
                    
                    if self.mensajes_esperados == 0:
                        self.canal_salida.envia((self.id_nodo, "BACK", "yes", mensaje[2]+1), [self.padre])
                    else:
                        vecinos_a_enviar = self.vecinos.copy()
                        try:
                            vecinos_a_enviar.remove(mensaje[0])
                        except ValueError:
                            pass # no se encuentra el vecino a quitar cuando el nodo mismo envio el mensaje (no es vecino de si mismo)
                        self.canal_salida.envia((self.id_nodo, "GO", mensaje[2]+1), vecinos_a_enviar)
                elif self.distancia > mensaje[2] + 1:
                    self.padre = mensaje[0]; self.hijos = list(); self.distancia = mensaje[2] + 1
                    self.mensajes_esperados = len(self.vecinos) - 1
                    
                    if self.mensajes_esperados == 0:
                        self.canal_salida.envia((self.id_nodo, "BACK", "yes", self.distancia), [self.padre])
                    else:
                        vecinos_a_enviar = self.vecinos.copy()
                        try:
                            vecinos_a_enviar.remove(mensaje[0])
                        except ValueError:
                            pass # no se encuentra el vecino a quitar cuando el nodo mismo envio el mensaje (no es vecino de si mismo)
                        self.canal_salida.envia((self.id_nodo, "GO", mensaje[2]+1), vecinos_a_enviar)
                else:
                    self.canal_salida.envia((self.id_nodo, "BACK", "no", mensaje[2]+1), [self.padre])
                
            if mensaje[1] == "BACK":
                if mensaje[3] == self.distancia + 1:
                    if mensaje[2] == "yes": self.hijos.append(mensaje[0])
                    self.mensajes_esperados -= 1
                    if self.mensajes_esperados == 0:
                        if self.padre != self.id_nodo:
                            self.canal_salida.envia((self.id_nodo, "BACK", "yes", self.distancia), [self.padre])
                            