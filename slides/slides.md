---
author: Gabriel Rodríguez & Stephen Campbell
title: Messaging Bridge
subtitle: A Rosetta Stone for Messaging Apps
date: May 7, 2025
---

# bla bla

Hello world

## bla bla

* bla bla
* bla bla
* bla bla

---

* bla bla
* bla bla

# Usage

## Config file

## Live demo

# Implementation

## The bridge

```scheme
(define (handle-event! event)
  (if (chat-event? event)
    (unless (bridged-event? event) ;; Crucial to avoid infinite loops
      (when (message-event? event)
        (let* ((chat (event-chat event))
	       (equivalent-chats (linked-chats-get chat)))
          (for-each (lambda (other-chat)
		      (bridge-message! event other-chat))
                    equivalent-chats))))
    (%default-event-handler event)))
```

## Clients

## Config

## Generic procedures