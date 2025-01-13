import simpy
from Nodo import *
from Canales.CanalRecorridos import *
from random import randint


class NodoDFS(Nodo):
    ''' Implementa la interfaz de Nodo para el algoritmo DFS.'''

    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida, num_nodos):
        ''' 
        Constructor de NodoDFS.
        
        Args:
            id_nodo (int): Identificador del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (Canal): Canal de entrada para recibir mensajes.
            canal_salida (Canal): Canal de salida para enviar mensajes.
            num_nodos (int): Número total de nodos en la red.
        '''
        super().__init__(id_nodo, vecinos, canal_entrada, canal_salida)
        self.padre = self.id_nodo
        self.hijos = []
        self.eventos = []
        self.reloj = [0] * num_nodos

    def dfs(self, env):
        ''' 
        Implementa el algoritmo DFS para el nodo.
        
        Args:
            env (simpy.Environment): Entorno de simulación de SimPy.
        '''
        
        # Si el nodo es el nodo raíz (id_nodo == 0)
        if self.id_nodo == 0:
            # Inicializa el primer hijo y actualiza el reloj y eventos
            self.hijos = [self.vecinos[0]]
            self.reloj[self.id_nodo] += 1
            self.eventos.append((self.reloj, "E", tuple({self.id_nodo}), self.id_nodo, self.vecinos[0]))
            # Envía el mensaje "GO" al primer vecino
            self.canal_salida.envia(("GO", {self.id_nodo}, self.id_nodo, self.reloj), [self.vecinos[0]])
        
        while True:
            # Espera un tiempo aleatorio entre 1 y 5 unidades de tiempo
            yield env.timeout(randint(1,5))
            mensaje = yield self.canal_entrada.get()
            tipo = mensaje[0]
            visitados = mensaje[1]
            p_j = mensaje[2]
            R_j = mensaje[3]
            
            # Actualiza el reloj manteniendo el que tenga un tiempo mayor en cualquier posición del arreglo
            self.reloj = max([self.reloj, R_j], key=max)
            self.reloj[self.id_nodo] += 1
            self.eventos.append((self.reloj, "R", tuple(visitados), p_j, self.id_nodo))
            
            if tipo == "GO":
                # Actualiza el padre del nodo
                self.padre = p_j
                
                if set(self.vecinos).issubset(visitados):
                    # Si todos los vecinos han sido visitados, envía un mensaje "BACK" al padre
                    self.reloj[self.id_nodo] += 1
                    self.eventos.append((self.reloj, "E", tuple(visitados | {self.id_nodo}), self.id_nodo, self.padre))
                    self.canal_salida.envia(("BACK", visitados | {self.id_nodo}, self.id_nodo, self.reloj), [self.padre])
                else:
                    # Selecciona al primer vecino no vistado, lo asigna como hijo y envía un mensaje "GO"
                    s = list(set(self.vecinos) - visitados)[0]
                    self.hijos = [s]
                    self.reloj[self.id_nodo] += 1
                    self.eventos.append((self.reloj, "E", tuple(visitados | {self.id_nodo}), self.id_nodo, s))
                    self.canal_salida.envia(("GO", visitados | {self.id_nodo}, self.id_nodo, self.reloj), [s])
                
            elif tipo == "BACK":
                if set(self.vecinos).issubset(visitados):
                    if self.padre == self.id_nodo:
                        # Si el nodo es la raíz y todos los vecinos han sido visitados, termina
                        break
                    else:
                        # Si no es la raíz, envía un mensaje "BACK" al padre
                        self.reloj[self.id_nodo] += 1
                        self.eventos.append((self.reloj, "E", tuple(visitados), self.id_nodo, self.padre))
                        self.canal_salida.envia(("BACK", visitados, self.id_nodo, self.reloj), [self.padre])
                else:
                    # Selecciona al siguiente vecino no visitado, lo asigna como hijo y envía un mensaje "GO"
                    t = list(set(self.vecinos) - visitados)[0]
                    self.hijos.append(t)
                    self.reloj[self.id_nodo] += 1
                    self.eventos.append((self.reloj, "E", tuple(visitados), self.id_nodo, t))
                    self.canal_salida.envia(("GO", visitados, self.id_nodo, self.reloj), [t])