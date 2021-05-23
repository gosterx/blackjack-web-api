package com.evolution.project.domain.user

import io.circe.generic.JsonCodec

@JsonCodec
case class UserJoinParams(userName: String, tableName: String)
