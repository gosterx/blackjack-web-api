package com.evolution.project.domain.game

import io.circe.generic.JsonCodec

@JsonCodec
case class HitParams(tableName: String, userName: String)
