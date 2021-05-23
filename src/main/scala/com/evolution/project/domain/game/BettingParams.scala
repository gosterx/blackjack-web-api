package com.evolution.project.domain.game

import io.circe.generic.JsonCodec

@JsonCodec
case class BettingParams(tableName: String, userName: String, bet: Int)
